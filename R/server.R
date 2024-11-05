server_app <- function() {
  require(shiny)
  require(seastar)
  require(foam)
  require(dplyr)
  require(ggplot2)
  require(ggthemes)
  require(writexl)
  library(ggplot2)
  library(dplyr)
  library(fs)
  library(readxl)
  library(plates)

  shinyServer(function(input, output) {
    observeEvent(input$dirsel, {
      E <- new.env()
      E$dir <- choose.dir()
      E$data <- barval::grab_validation_data(E$dir)


      E$median_target <- median(E$data$pH_target)
      E$Lot <- unique(E$data$Lot)
      ## KSV
      # message("ksv data")
     E$ksv <- tryCatch({
        filter(E$data,is.na(KSV)) %>%
              group_by(O2_target) %>%
              mutate(
                modz = modified_z_score(KSV),
                cut = modz_cuts(KSV),
                use = modz <= cut
              ) %>%
              ungroup() %>%
              filter(use) %>%
              summarize(
                AVG_KSV = mean(KSV, na.rm = TRUE),
                AVG_F0 = mean(F0, na.rm = TRUE),
                Median_ksv = median(KSV, na.rm = TRUE),
                Median_F0 = median(F0, na.rm = TRUE),
                O2_target = median(O2_target, na.rm = TRUE)
              )}, error = function(e) NULL)
      ### GAIN
      # message("profile gain data")
      gain_lm <- tryCatch({lm(Gain ~ pH_target, data = E$data)},
                           error = function(e) NULL)


      E$coeffs <- tryCatch({tibble(
        target = median(unique(E$data$pH_target)),
        slope = round(coef(gain_lm)[2], 6),
        intercept = round(coef(gain_lm)[1], 6),
        Gain = (target * slope) + intercept,
        rsquared = round(summary(gain_lm)$r.squared, 6)
      )}, error = function(e) NULL)


      E$plot$pH_lm <-
        tryCatch({
        filter(E$data, is.na(KSV)) %>%
        ggplot(., aes(pH_target, Gain)) +
        geom_point(alpha = 0.2) +
        geom_smooth(method = "lm") +
        theme_bw() +
        ggtitle(
          label = unique(E$data$Lot),
          subtitle = paste0(
            "Rsquared: ",
            E$coeffs$rsquared,
            "\n",
            "eq: Gain = (Target * ",
            E$coeffs$slope,
            ") +",
            E$coeffs$intercept
          )
        )}, error = function(e) NULL)


      ##### LED & Gain table
      message("set tables")
      E$gain_led_table <-
        tryCatch({
        filter(E$data, !is.na(pH_target)) %>%
        filter(.,!is.na(Gain)) %>%
        group_by(., pH_target) %>%
        summarise(
          .,
          pH_LED_AVG = mean(pH.LED),
          O2_LED_AVG = mean(O2.LED),
          Gain_AVG = mean(Gain,
                          na.rm = T),
          Gain_SD = sd(Gain),
          Gain_minus3SD = Gain_AVG -
            (3 * Gain_SD),
          Gain_plus3SD = Gain_AVG + (3 *
                                       Gain_SD)
        )}, error = function(e) NULL)

      ### Save output
        xlist<-  list(data=E$data,
                 Gain_table=E$gain_led_table,
                 pH_coefficients = E$coeffs,
                 E$ksv)
      message("save outputs")
      writexl::write_xlsx(
        x = Filter(Negate(is.null), xlist),
        path = file.path(file.path(
          E$dir,
          paste0(E$Lot, "_coefficient_summary.xlsx")
        ))
      )
      ggsave(
        file.path(E$dir, "coeffplot.png"),
        E$plot$pH_lm,
        width = 10,
        height = 6
      )
      message("done,kill object")
      E <- NULL
    })
  })
}
