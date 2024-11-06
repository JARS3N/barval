plot_lm<-function(data,coefs){
#takes E$data as argument
tryCatch({
            filter(data, !is.na(KSV)) %>%
              ggplot(aes(pH_target, Gain)) +
              geom_point(alpha = 0.2) +
              geom_smooth(method = "lm") +
              theme_bw() +
              ggtitle(
                label = unique(E$data$Lot),
                subtitle = paste0(
                  "Rsquared: ", coefs$rsquared, "\n",
                  "eq: Gain = (Target * ", coefs$slope, ") + ",
                  coefs$intercept
                )
              )
          }, error = function(e) NULL)
}
