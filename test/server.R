
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

TestFun <- function(number1, number2){
  
  x <- data.frame(runif(100, min=number1, max=number2))
  

  names(x) <- "val"
  
  return(x)
}



shinyServer(function(input, output) {
  
   y <- reactive({TestFun(as.numeric(input$number1), as.numeric(input$number2))})
   #y <- reactive({TestFun(10, 30)})
   
# 
  output$distPlot <- renderPlot({
    browser()
#     
#     barplot(y()$val)
  
  z <- y()$val
  barplot(z)
    



  })

})
