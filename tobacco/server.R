
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

source("tobacco.R")

library(maps)

library(shiny)

shinyServer(function(input, output) {


  
  #############
  output$plot1 <- renderPlot({

    if(input$yvar == 0){
      BarFun(var = comb[,as.numeric(input$xvar)],
             by_var = NULL, 
             recode_var = NULL, 
             ref = NULL,
             cex.names = input$cex.names, 
             las = input$las, 
             legend = input$legend, 
             rain = input$rain,
             border = "black", 
             percent = input$percent,
             legend.cex = input$legend.cex, 
             legend.title = input$legend.title,
             err.cex = input$err.cex,
             ylab = input$ylab)
    } else {
      BarFun(var = comb[,as.numeric(input$xvar)],
             by_var = comb[,as.numeric(input$yvar)],
             recode_var = NULL, 
             ref = NULL,
             cex.names = input$cex.names, 
             las = input$las, 
             legend = input$legend, 
             rain = input$rain,
             border = "black", 
             percent = input$percent,
             legend.cex = input$legend.cex, 
             legend.title = input$legend.title,
             err.cex = input$err.cex,
             ylab = input$ylab)
      
    }
    
    })
  #############
  output$plot2 <- renderPlot({
   
    mymap <- map("county", "florida")
    title(main = "Under construction")
    
  })
  
  

})
