grab_coefs<-function(model,target){
# model=gain_lm
# target=E$gain$pH_target
  tryCatch({
            tibble(
              target = target,
              slope = round(coef(model)[2], 6),
              intercept = round(coef(model)[1], 6),
              Gain = (target * slope) + intercept,
              rsquared = round(summary(model)$r.squared, 6)
            )
          }, error = function(e) NULL)

}
