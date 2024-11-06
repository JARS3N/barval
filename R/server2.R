server2 <- function() {
  library(shiny)
  library(shinyFiles)
  library(dplyr)
  library(ggplot2)
  library(writexl)
  library(fs)
  library(readxl)
  library(plates)
  
  shinyServer(function(input, output, session) {
    # Set up the specific directory as the root
  volumes <- c("Starting Directory" = 
                 "G:/00 AA SENSOR CRTG MFG/02 FLOUR BARCODE_QUALIFICATION/BARCODE",
               "Home"="~"
               )
    
    # Initialize shinyFiles for directory selection
    #shinyDirChoose(input, "directory", roots = volumes, session = session)
    shinyDirChoose(input, "blue", roots = volumes, session = session)
    # Observe the folder selection and display the path
    observe({
      if (!is.null(input$blue)) {
        # Parse selected directory path
        selected_dir <- parseDirPath(volumes, input$blue)
        
        # Check if the selected directory is valid before proceeding
        if (is.null(selected_dir) || identical(selected_dir, "")) {
          print("Debug: No directory selected or path is empty")
          output$selected_path <- renderPrint({ "No directory selected" })
          return(NULL)  # Exit if no valid directory is selected
        }
        
        # Convert the parsed path to a character string and display it
        output$selected_path <- renderPrint({ as.character(selected_dir) })
        
        # Process files in the selected directory if valid
        E <- new.env()
        E$dir <- selected_dir
        E$data <- tryCatch({
          barval::grab_validation_data(E$dir)
        }, error = function(e) NULL)
        cat("\ndoes E$data exists: ")
        print(is.null(E$data))
        if (!is.null(E$data) && nrow(E$data) > 0) {
          # make sure there is data
          E$median_target <- median(E$data$pH_target)
          E$Lot <- unique(E$data$Lot)
          
          # KSV calculation
          E$ksv <- calculate_ksv(E$data)
          
          # Gain calculation
          gain_lm <- tryCatch({
            lm(Gain ~ pH_target, data = E$data)
          }, error = function(e) NULL)
          #print(summary(gain_lm))
          E$coeffs <- barval::grab_coefs(gain_lm,E$median_target)
          # Plotting
          # X<<-E$data
          # Y<<-E$coeffs
          E$plot$pH_lm <- barval::plot_lm(E$data,E$coeffs)
          print(E$plot$pH_lm)
          # Gain LED table
          E$gain_led_table <- barval::generate_led_table(E$data)
          
          # Save results to Excel
          xlist <- list(
            data = E$data,
            Gain_table = E$gain_led_table,
            pH_coefficients = E$coeffs,
            ksv = E$ksv
          )
          
          xlpath<-file.path(E$dir, paste0(E$Lot, "_coefficient_summary.xlsx"))
          writexl::write_xlsx(
            x = Filter(Negate(is.null), xlist),
            path = xlpath
          )
          
          # Save the plot
          plotpath<-file.path(E$dir, "coeffplot.png")
          ggsave(
            plotpath,
            E$plot$pH_lm,
            width = 10,
            height = 6
          )
          wb <- loadWorkbook(xlpath)
          addWorksheet(wb, "Plot")
          insertImage(wb, sheet = "Plot", 
                      file = plotpath, 
                      width = 10, height = 8, 
                      startRow = 1, startCol = 1)
          saveWorkbook(wb, xlpath, overwrite = TRUE)
        #  unlink(plotpath)
        }
      }
    })
  })
}
