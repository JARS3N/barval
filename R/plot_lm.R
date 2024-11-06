plot_lm<-function(u){
#takes E$data as argument
tryCatch({
            filter(u, !is.na(KSV)) %>%
              ggplot(aes(pH_target, Gain)) +
              geom_point(alpha = 0.2) +
              geom_smooth(method = "lm") +
              theme_bw() +
              ggtitle(
                label = unique(E$data$Lot),
                subtitle = paste0(
                  "Rsquared: ", E$coeffs$rsquared, "\n",
                  "eq: Gain = (Target * ", E$coeffs$slope, ") + ",
                  E$coeffs$intercept
                )
              )
          }, error = function(e) NULL)
}
