
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
  
  output$table <- renderDataTable({
    if(!input$show_payments){
      x <- df[which(df$name == input$student),]
      #x$date <- as.character(x$date)
      x$time <- paste0(x$time_hour,":", x$time_minute)
      x <- x[,c("date", "time", "pre_paid", "price")]
      x$time[which(nchar(x$time) == 3)] <- paste0(x$time[which(nchar(x$time) == 3)], "0")
      x$date <- format(x$date, format = "%b %d, %Y")
      x$price <- paste0("$", x$price)
      row.names(x) <- NULL
      x
    } else{
      x <- payment_df(input$student)
      x$amount <- paste0("$", x$amount)
      names(x)[3] <- "amount paid"
      #x$date <- as.character(x$date)
     
      row.names(x) <- NULL
      x
    }

  }, options = list(paging = FALSE, searching = FALSE))


})
