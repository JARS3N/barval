generate_led_table<-function(u){
# E$data as argument
tryCatch({
            filter(u, !is.na(pH_target) & !is.na(Gain)) %>%
              group_by(pH_target) %>%
              summarize(
                pH_LED_AVG = mean(pH.LED, na.rm = TRUE),
                O2_LED_AVG = mean(O2.LED, na.rm = TRUE),
                Gain_AVG = mean(Gain, na.rm = TRUE),
                Gain_SD = sd(Gain, na.rm = TRUE),
                Gain_minus3SD = Gain_AVG - (3 * Gain_SD),
                Gain_plus3SD = Gain_AVG + (3 * Gain_SD)
              )
          }, error = function(e) NULL)

}
