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
      E$ksv <- filter(E$data,!is.na(KSV)) %>%
        group_by(O2_target) %>% 
        mutate(modz=barval::modified_z_score(KSV),
               cut = barval::modz_cuts(KSV),
               use = modz<=cut
               ) %>% 
        ungroup() %>% 
        filter(use) %>% 
        summarize(
          .,
          AVG_KSV = mean(KSV),
          AVG_F0 = mean(F0),
          Median_ksv = median(KSV),
          Median_F0 = median(F0)
        )
      ### GAIN
      # message("profile gain data")
      gain_lm <- lm(Gain ~ pH_target, data = E$data)
      
      
      E$coeffs <- tibble(
        target = median(unique(E$data$pH_target)),
        slope = round(coef(gain_lm)[2], 6),
        intercept = round(coef(gain_lm)[1], 6),
        Gain = (target * slope) + intercept,
        rsquared = round(summary(gain_lm)$r.squared, 6)
      )
      
      
      E$plot$pH_lm <-
        filter(E$data, is.na(KSV)) %>%
        ggplot(., aes(pH_target, Gain)) +
        geom_point(alpha = 0.2) +
        geom_smooth(method = "lm") +
        theme_bw() +
        ggtitle(
          label = unique(E$data$raw$Lot),
          subtitle = paste0(
            "Rsquared: ",
            E$coeffs$rsquared,
            "\n",
            "eq: Gain = (Target * ",
            E$coeffs$slope,
            ") +",
            E$coeffs$intercept
          )
        )
      
      
      ##### LED & Gain table
      message("set tables")
      E$gain_led_table <-
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
        )
      
      ### Save output
      message("save outputs")
      writexl::write_xlsx(
        x = list(E$data,
                 E$gain_led_table,
                 E$coeffs,
                 E$ksv),
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
