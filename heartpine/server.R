
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

source("get_data.R")
source("helpers.R")
shinyServer(function(input, output) {

  output$distPlot <- renderPlot({


    visualize(input$student, data = df, show_vals = input$show_vals)
    
  })
  
  output$table <- renderTable({
    x <- df[which(df$name == input$student),]
    x$date <- as.character(x$date)
    x$time <- paste0(x$time_hour,":", x$time_minute)
    x <- x[,c("date", "sibling", "time", "pre_paid", "price")]
    x
  })

})
