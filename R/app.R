app<-function(){
library(barval)
library(shiny)
library(shinyFiles)
library(openxlsx)
 # shiny::shinyApp(barval::ui_app(),barval::server_app())
  shiny::shinyApp(barval::ui2(),barval::server2())
}
