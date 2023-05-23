grab_validation_data<-function(directory=choose.dir()){
  gaintabs <- fs::dir_ls(directory,regexp = "GAINTAB_.*[.]xlsx$")
  wavefiles <- fs::dir_ls(directory,regexp = "(asyr|xflr)$")
  tabs <- try(purrr::map_df(gaintabs,process_gaintab))
  waves <- try(purrr::map_df(wavefiles,process_wave_qc))
  dplyr::bind_rows(list(waves,tabs))
}
