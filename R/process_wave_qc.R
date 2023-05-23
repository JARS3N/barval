process_wave_qc<-function(k){
  u<-foam::new(k)
  left_join(u$calibration,u$summary) %>%
    mutate(.,Lot=paste0(u$type,u$lot),
           sn = u$sn,
           Inst=u$Inst,
           assay = u$assay,
           O2_target = u$O2_coefs$target,
           pH_target = u$pH_coefs$target,
           file = u$file,
           source = "wave"
    ) %>%
      ungroup() %>%
    mutate(Well = plates::num_to_well(Well,max(Well)))
}
