app<-function(){
  shiny::shinyApp(barval::ui_app(),barval::server_app(),launch.browser=T)
}
