
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

require(shiny)
require(rCharts)

shinyServer(function(input, output) {
  
  source("farmers.R")
  source("schools.R")
  
  
  ########
  mydata <- reactive({
    farm[which(farm$month == input$month),]
  })
  
  
  ########
  mydata_goodcolumns <- reactive({
    farm2[which(farm2$month == input$month),]
  })



  
  ############
  # LEAFLET CHART - FARMS
  
  output$myChart2 <- renderMap({
    mymap <- Leaflet$new()
    mymap$tileLayer(provider = "Stamen.TonerLite")
    mymap$setView(c(27.85, -81.3), zoom = 6)
    mymap$enablePopover(TRUE)
#     mymap$marker(c(51.5, -0.09), bindPopup = "Hi. I am a popup")
#     mymap$marker(c(51.495, -0.083), bindPopup = "Hi. I am another popup")
#     
    mymap$set(dom = 'myChart2')
    for (i in which(!is.na(farm_geo$lat & is.na(farm_geo$lon)))){
      mymap$marker(c(farm_geo$lat[i],
                     farm_geo$lon[i]),
                   bindPopup = paste0(as.character(farm_geo$Farm.name.[i]),
                                      " (recruitment date: ", 
                                      ifelse(is.na(farm_geo$timestamp[i]), 
                                             "unknown",
                                             format(as.Date(farm_geo$timestamp[i],
                                                            format = "%Y-%m-%d"),
                                                    "%b %d %Y")),") ",
                                      "Nearest 5 schools: ",
                                      as.character(farm_geo$nearest5schools[i])))
      
    }
mymap
  })


############
# LEAFLET CHART SCHOOLS

output$myChart3 <- renderMap({
  mymap <- Leaflet$new()
  mymap$tileLayer(provider = "Stamen.TonerLite")
  mymap$setView(c(27.85, -81.3), zoom = 6)
  mymap$enablePopover(TRUE)
  #     mymap$marker(c(51.5, -0.09), bindPopup = "Hi. I am a popup")
  #     mymap$marker(c(51.495, -0.083), bindPopup = "Hi. I am another popup")
  #     
  mymap$set(dom = 'myChart3')
  for (i in 1:nrow(schools)){
    mymap$marker(c(schools$LATITUDE[i],
                   schools$LONGITUDE[i]),
                 bindPopup = paste0(as.character(schools$SCHOOL_NAME_SHORT[i]),
                                    " (email: ", 
                                    ifelse(is.na(schools$EMAIL_ADDRESS_PRI[i]), 
                                           "unknown",
                                           as.character(schools$EMAIL_ADDRESS_PRI[i])),")",
                                    " Closest 5 farms: ", schools$nearest5farms[i]))
    
  }
  mymap
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
output$plot3 <- renderPlot({
  par(mfrow=c(1,1))
  par(mar=c(0,0,0,0))
  par(oma=c(0,0,0,0))
    
    #mycols <- adjustcolor(colorRampPalette(brewer.pal(8, "Dark2"))(nrow(farm)), alpha.f=0.8)
  mysize <- ifelse(farm$What.is.your.farm.s.total.harvestable.acreage. == "", 1,
                   ifelse(farm$What.is.your.farm.s.total.harvestable.acreage. == "1-9", 2,
                          ifelse(farm$What.is.your.farm.s.total.harvestable.acreage. == "1.5 Acres", 2,
                                 ifelse(farm$What.is.your.farm.s.total.harvestable.acreage. == "10-49", 3,
                                        ifelse(farm$What.is.your.farm.s.total.harvestable.acreage. == "50-179", 4,
                                               ifelse(farm$What.is.your.farm.s.total.harvestable.acreage. == "180-499", 5,
                                                      ifelse(farm$What.is.your.farm.s.total.harvestable.acreage. == "500-999", 6,
                                                             ifelse(farm$What.is.your.farm.s.total.harvestable.acreage.  == "1000+", 7,
                                                                    1)))))))) 
  
  mycols <- ifelse(farm$Have.you.ever.tried.to.sell.to.schools. == "", "black",
                   ifelse(farm$Have.you.ever.tried.to.sell.to.schools. == "No", "darkred",
                          ifelse(farm$Have.you.ever.tried.to.sell.to.schools. == "Yes", "darkgreen",
                                 "black")))
  mycols <- adjustcolor(mycols, alpha.f=0.3)
  
    map("county", "fl", fill=TRUE, col="white", border="darkgrey")
    
    for(i in 1:nrow(farm)){
      points(farm$lon[i], farm$lat[i], 
             col=mycols[i],
             pch=16,cex = mysize[i] / 1.5)
    }
  
  legend("bottomleft", pch=16, col=adjustcolor(c("black", "darkred", "darkgreen"), alpha.f=0.7),
         legend=c("Unknown", "No", "Yes"),
         title = "Ever tried to sell to schools?",
         bty="n", border=FALSE, cex=0.9)
  
  legend("left", pch=16, col=adjustcolor("grey", alpha.f=0.5), pt.cex=(1:7)/1.5,
         legend=c("Unknown", "1-9", "10-49", "50-179", "180-499", "500-999", "1000+"),
         title = "Total harverstable acreage",
         bty="n", border=FALSE, cex=0.9, y.intersp=1.6, ncol=2)
  
  
    
#     barplot(1:10, col="white", border="white", xlab=NA, ylab=NA, xaxt="n", yaxt="n")
#     legend("center", col=mycols, pch=16, border=FALSE, bty="n",
#            legend=farm$Farm.name., cex=0.7, pt.cex=1.5,
#            ncol=2)

  
  
}, height=600)
  
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

############
output$table3 <- renderDataTable({
  x <- as.data.frame(farm2)
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
