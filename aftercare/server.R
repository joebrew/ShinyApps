
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  #setwd("C:/Users/BrewJR/Documents/ShinyApps/aftercare")
  source("helper.R")

  output$plot1 <- renderPlot({
    
    teacherPay <- input$teacher
    studentCost <- input$student
    
    for (i in ts$day){
      for (j in c(30,60,90,120)){
        income <- Hourfy(ts[which(ts$day == i),paste0("x", j)])*studentCost
        expense <- Hourfy(j)*teacherPay
        roi <- income - expense
        
        ts[which(ts$day == i), paste0("roi", j)] <- roi
        
        
      }
    }
    
    ts2 <- ts[, grepl("roi|day", colnames(ts))]
    
    
    colnames(ts2) <- c("day", "3:30", "4:00", "4:30", "5:00")
    ts2
    
    
    PlotFun <- function(day){
      num <- as.numeric(ts2[which(ts$day == day),2:5])
      bp <- barplot(num,
                    names.arg= c("3:30", "4:00", "4:30", "5:00"),
                    main=day,
                    cex.axis =0.6)
      text(bp[,1], 
           y = num,
           pos = ifelse(num > 0,1,3),
           labels= num)
      abline(h=0)
    }
    
    par(mfrow=c(5,1))
    par(mar=c(4,4,1,1))
    for (i in days){
      PlotFun(i)
    }

  })

  output$plot2 <- renderPlot({
    barplot(1:10)
  })
  
  output$table1 <- renderTable({
    
    teacherPay <- input$teacher
    studentCost <- input$student
    
    for (i in ts$day){
      for (j in c(30,60,90,120)){
        income <- Hourfy(ts[which(ts$day == i),paste0("x", j)])*studentCost
        expense <- Hourfy(j)*teacherPay
        roi <- income - expense
        
        ts[which(ts$day == i), paste0("roi", j)] <- roi
        
        
      }
    }
    
    ts2 <- ts[, grepl("roi|day", colnames(ts))]
    colnames(ts2) <- c("day", "3:30", "4:00", "4:30", "5:00")
    
    ts2
    
  })
  
    
  
})
