
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(knitr)
source("helpers.R")
source("get_data.R")
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
      if(nrow(x) > 0 ){
        x$amount <- paste0("$", x$amount)
        names(x)[3] <- "amount paid"
        #x$date <- as.character(x$date)
        
        row.names(x) <- NULL
      }
      x
      
    }

  }, options = list(paging = FALSE, searching = FALSE))


  
  
  
  # Define downloadHandler object
  output$downloadPDF <-
    downloadHandler(filename = "report.pdf",
                    content = function(file){
                      # generate PDF
                      Sweave2knitr("report_sweave.Rnw", "report.Rnw")
                      knit2pdf("report.Rnw")
                      
                      # copy pdf to 'file'
                      file.copy("report.pdf", file)
                      
                      # delete generated files
                      file.remove("report.pdf", "report.tex",
                                  "report.aux", "report.log")
                      
                      # delete folder with plots
                      unlink("figure", recursive = TRUE)
                    },
                    contentType = "application/pdf"
    )
  
  # Define downloadHandler object MASTER
  output$downloadPDF_master <-
    downloadHandler(filename = "report_master.pdf",
                    content = function(file){
                      # generate PDF
                      Sweave2knitr("report_master_sweave.Rnw", "report_master.Rnw")
                      knit2pdf("report_master.Rnw")
                      
                      # copy pdf to 'file'
                      file.copy("report_master.pdf", file)
                      
                      # delete generated files
                      file.remove("report_master.pdf", "report_master.tex",
                                  "report_master.aux", "report_master.log")
                      
                      # delete folder with plots
                      unlink("figure", recursive = TRUE)
                    },
                    contentType = "application/pdf"
    )
  
  
})
