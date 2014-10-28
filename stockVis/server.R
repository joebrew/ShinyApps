library(shiny)
library(quantmod)
source("helper.R")


shinyServer(function(input, output) {
  
  dataInput <- reactive({  
    if(input$get == 0) return(NULL)
    
    isolate({
      getSymbols(input$symb, src = "yahoo", 
                 from = input$dates[1],
                 to = input$dates[2],
                 auto.assign = FALSE)
    })
  })
  
  finalInput <- reactive({
    if (!input$adjust) return(dataInput())
    adjust(dataInput())
  })
  
  output$plot <- renderPlot({
    if(input$get == 0) return(NULL)
    
    chartSeries(finalInput(), theme = chartTheme("white"), 
                type = "line", log.scale = input$log, TA = NULL)
  })
})