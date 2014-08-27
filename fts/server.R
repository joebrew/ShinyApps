
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  
  source("farmers.R")
  
  ########
  mydata <- reactive({
    farm[which(farm$month == input$month),]
  })
  
  
  ########
  mydata_goodcolumns <- reactive({
    farm2[which(farm2$month == input$month),]
  })
  


  ############
  output$plot1 <- renderPlot({
    par(mfrow=c(1,2))
    
    if(nrow(mydata()) > 0){
      
      mycols <- adjustcolor(colorRampPalette(brewer.pal(8, "Dark2"))(nrow(mydata())), alpha.f=0.8)
      
      map("county", "fl", fill=TRUE, col="grey", border="white")
      
      for(i in 1:nrow(mydata())){
        points(mydata()$lon[i], mydata()$lat[i], 
               col=mycols[i],
               pch=16)
      }
      
      barplot(1:10, col="white", border="white", xlab=NA, ylab=NA, xaxt="n", yaxt="n")
      legend("center", col=mycols, pch=16, border=FALSE, bty="n",
             legend=mydata()$Farm.name., cex=0.7, pt.cex=1.5,
             ncol=2)
   }else{
     barplot(1:10, col="white", border="white", xlab=NA, ylab=NA, xaxt="n", yaxt="n")
     text(5,5, labels="No data collected this month")
   }
    

  })
  
  ############
  output$plot2 <- renderPlot({
    
    if(nrow(mydata()) > 0){
      
barplot(1:10)
text(x=4, y=5, labels="this plot is just a \n placeholder")

    }else{
      plot(1:10, 1:10, type="n", xlab=NA, ylab=NA, xaxt="n", yaxt="n")
      text(5,5, labels="No data collected this month")
    }
    
    
  })
  
  ############
  output$table1 <- renderDataTable({
    x <- as.data.frame(mydata())
    x <- x[,c("Farm.name.", "Farmer.name.")]

  })
  
  ############
  output$table2 <- renderDataTable({
    x <- as.data.frame(mydata_goodcolumns())
    #x <- x[,names(july)]
    
  })

###########

# output$downloadData <- downloadHandler(
#   filename = function() { paste(input$month, '.csv', sep='') },
#   content = function(file) {
#     write.csv(mydata(), file)
#   }
# )

###########

output$downloadData1 <- downloadHandler(
  
  
  filename = function() { paste(input$month, '.csv', sep='') },
  content = function(file) {
    write.csv(mydata_goodcolumns(), file)
  }
)


###########

output$downloadData2 <- downloadHandler(

  
  filename = function() { paste("all_months", '.csv', sep='') },
  content = function(file) {
    write.csv(farm2, file)
  }
)

})
