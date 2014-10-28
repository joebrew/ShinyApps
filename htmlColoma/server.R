library(shiny)

#source the budgetFun file
source("~/ShinyApps/htmlColoma/helper.R")



soFar <- df$tot[which(df$date == Sys.Date())] / 60


# Define server logic for slider examples
shinyServer(function(input, output) {
  
  output$text1 <- renderText({ 
    
    #paste(input$tot.goal)
    
    paste(
      "So far, you've done", soFar, "hours",
      "- not bad!",
      ifelse(input$weekend, 
                 "Working weekends", 
                 "Without working weekends") , 
      "with a daily workload of",
      input$perDay, "hours per day",
          "you'll reach your goal of having done",
          input$tot.goal, "hours",
          "in about",
      round(ifelse(input$weekend,
             (input$tot.goal -
                soFar) /
               input$perDay,
             ((input$tot.goal -
                 soFar) /
                input$perDay)/(5/7)
             ), digits=0),
      
      "days")
    
   })
  
  output$plot1 <- renderPlot({
    
    pie(c(max(df$tot, na.rm=T)/60,
          (input$tot.goal - (max(df$tot, na.rm=T)/60))),
        labels=c("Done", "To do"))
#     plot(df$date, df$tot.goal,
#          type="l",
#          col=adjustcolor("darkblue", alpha.f=0.5),
#          lwd=2,
#          xlab="Date",
#          ylab="Total Minutes",
#          cex.axis=0.7)   
#     lines(df$date, df$tot,
#           col=adjustcolor("darkred", alpha.f=0.7),
#           lwd=1)
    
    
  })

})