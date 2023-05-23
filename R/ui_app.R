ui_app<-function(){
  ###########
  library(shiny)

  shinyUI(fluidPage(

    # Application title
    titlePanel("Sensor Validation Coefficients"),

    sidebarLayout(
      sidebarPanel(
        shiny::actionButton('dirsel',"Select Directory")
      ),

      mainPanel(

      )
    )
  ))
  ###########
}
