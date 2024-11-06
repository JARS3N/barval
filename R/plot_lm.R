
plot_lm <- function(data, coefs) {
  #takes E$data as argument
  tryCatch({
    data %>%
      ggplot(aes(x = as.factor(pH_target), y = Gain)) +  # Treat pH_target as discrete
      geom_violin(fill = "lightblue", alpha = 0.3) +  # Violin plot for distribution
      geom_point(position = position_jitter(width = 0.05), alpha = 0.33) +  # Overlayed points with jitter
      geom_smooth(
        aes(group = 1),
        method = "lm",
        se = FALSE,
        color = "blue"
      ) +  # Overall linear fit
      theme_minimal() +
      labs(x = "pH Target", y = "Gain") +
      ggtitle(
        label = unique(data$Lot),
        subtitle = paste0(
          "Rsquared: ",
          coefs$rsquared,
          "\n",
          "eq: Gain = (Target * ",
          coefs$slope,
          ") + ",
          coefs$intercept
        )
      )
    
  }, error = function(e)
    NULL)
}
