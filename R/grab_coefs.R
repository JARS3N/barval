grab_coefs<-function(model,target){
# model=gain_lm
# target=E$gain$pH_target
  tryCatch({
            tibble(
              target = median(unique(target)),
              slope = round(coef(gain_lm)[2], 6),
              intercept = round(coef(gain_lm)[1], 6),
              Gain = (target * slope) + intercept,
              rsquared = round(summary(gain_lm)$r.squared, 6)
            )
          }, error = function(e) NULL)

}
