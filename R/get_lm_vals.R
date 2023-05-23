get_lm_vals <- function(u) {
  pH_data <- u %>%
    filter(assay == "gain") %>%
    select(Well, pH_target, Gain, pH.CalEmission) %>%
    group_by(pH_target) %>%
    mutate(
      modz = modified_z_score(Gain),
      cut = modz_cuts(Gain),
      clude = if_else(modz >= cut, "ex", "in")
    )
  lm <-
    filter(pH_data, clude == 'in') %>%
    lm(Gain ~ pH_target, .)

  coefs <- coef(lm)
  smry <- summary(lm)
  tibble(
    pH_target = median(unique(pH_data$pH_target)),
    slope = coefs[[2]],
    intercept = coefs[[1]],
    rsquared = smry$r.squared,
    rsquare_adjusted = smry$adj.r.squared,
  )
}
