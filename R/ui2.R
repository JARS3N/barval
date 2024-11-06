library(shiny)
library(shinyFiles)

ui2 <- function() {
   fluidPage(
    titlePanel("Select a Folder"),
    
    sidebarLayout(
      sidebarPanel(
        shinyDirButton("blue", "Select Folder", 
                       title = "Please select a folder:",
                       buttonType = "default", class = NULL),
        verbatimTextOutput("selected_path")
      ),
      mainPanel(p('Barcode Coefficient Generation'))
    )
  )
}
