library(shiny)
suppressPackageStartupMessages(library(googleVis))


shinyServer(function (input, output#, 
                      #session
) {
  
  counties_small <- read.csv("counties_small.csv")
  
 
  
  ########
  output$motionchart1 <- renderGvis({
    
    gvisMotionChart(counties_small, 
                    idvar="name", 
                    timevar="year",
                    xvar = "married_rate",
                    yvar = "lbw_rate",
                    colorvar = "gini",
                    sizevar = "total_births")
    
  })
  
 
  
  #outputOptions(output, "motionchart", suspendWhenHidden = FALSE)
  
  
  #   outputOptions(output, "edades", suspendWhenHidden = FALSE)
  #   outputOptions(output, "tabla1", suspendWhenHidden = FALSE)
  #   outputOptions(output, "tabla2", suspendWhenHidden = FALSE)
  #   outputOptions(output, "linech", suspendWhenHidden = FALSE)
})