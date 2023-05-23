process_gaintab<-function(k){
  # remove path extra from filename
  file <-basename(k)

  # read in 2 excel tabs
  res <- readxl::read_xlsx(k,sheet = 'Results')
  u<-readxl::read_xlsx(k,sheet='Resume')

  # get instrument
  whc <- grep("Serial number",unlist(res[,1]))
  inst <- unlist(res[whc,2])

  # renaming LED columns due to analyte
  analyte <- unique(u$Analyte)
  led_name<- paste0(analyte,".LED")

  # pull calibration
  calibration <-filter(u,Tick == "Calibration") %>%
    {setNames(.,gsub("Counts",led_name,names(.)))} %>%
    select(.,-Tick,-Analyte)

  # pull out measurement data
  out<- select(u,Well,Counts,Emission,Tick) %>%
    filter(.,Tick!="Calibration") %>%
    mutate(Tick=as.numeric(Tick)) %>%
    mutate(M = if_else(Tick>3,"M2","M1")) %>%
    group_by(Well,Emission,M) %>%
    summarize(counts=mean(Counts)) %>%
    tidyr::spread(.,M,counts) %>%
    ungroup() %>%
    left_join(.,calibration) %>%
    mutate(Well=plates::pad0(Well),
           file= file,
           Inst = inst,
           source= "gaintab",
           Lot = substr(file,1,6),
           sn = gsub("^.{7}|_GAINTAB_.+$","",file)
           )

  # adjust output for the determined assay (gain/ksv)
  if (analyte == "pH") {
    mutate(out,
           Gain = (Emission * M1 ^ -1) * (M1 - M2) * (800 ^ -1),
           assay = "gain") %>%
      rename(sorpH = M2,
             pH.CalEmission = M1,
             pH_target = Emission) %>%
      return()
  } else{
    rename(out,
           Amb = M1,
           F0 = M2,
           O2_target = Emission) %>%
      mutate(KSV = ((F0 / Amb) - 1) / foam::partial_pressure_ox(37)) %>%
      return()
  }

}
