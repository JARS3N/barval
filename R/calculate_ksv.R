calculate_ksv<-function(u){
#takes E$data as argument
  tryCatch({
            filter(u, !is.na(KSV)) %>%
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
              )
          }, error = function(e) NULL)
}
