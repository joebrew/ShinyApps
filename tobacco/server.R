
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

source("tobacco.R")

library(shiny)

shinyServer(function(input, output) {

  #############
  output$plot1 <- renderPlot({

    # generate bins based on input$bins from ui.R
    hist(rnorm(n = 1000, mean = 0, sd = 5))
  })
  #############
  output$plot2 <- renderPlot({
   barplot(1:10)
  })
  
  
  #############
  output$plot3 <- renderPlot({
    barplot(1:10)
  })
})
