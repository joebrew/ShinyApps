
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

source("tobacco.R")

library(maps)
library(rCharts)
library(shiny)
library(stringr)
library(rgdal)
library(classInt)
library(RColorBrewer)

shinyServer(function(input, output) {

  
  ########### INTERACTIVE MAP
  output$myChart2 <- renderMap({
    
    # WHOLE STATE
    if (!(input$county %in% as.character(fl$County))){
      mymap <- Leaflet$new()
      mymap$tileLayer(provider = "Stamen.TonerLite")
      mymap$setView(c(27.85, -81.3), zoom = 6)
      #points
      temp <- comb
      
      mymap$enablePopover(TRUE)
    
      mymap$set(dom = 'myChart2')
      for (i in which(!is.na(temp$lat) & !is.na(temp$lon))){
        mylabel <- paste0(str_replace_all(temp$Name.of.Employer[i], "[^[:alnum:]]", " "),
                          " || ",
                          "Sector: ",
                          ifelse(is.na(temp[i,11]),
                                 "unknown",
                                 temp[i,11]),
                          " || ",
                          "Interviewed: ",
                          format(as.Date(temp$Date.of.Interview[i],
                                         format = "%m/%d/%Y"),
                                 "%b %d %Y"),
                          " || ",
                          "Number of employees: ",
                          as.character(temp$X..employees.at.work.place[i]))
        
        #print(mylabel)
        mymap$marker(c(temp$lat[i],
                       temp$lon[i]),
                     #bindPopup = "<p> Hi. I am a popup. </p>")}
                     bindPopup = mylabel)}
      
      
    }else{ # JUST COUNTY
      mymap <- Leaflet$new()
      mymap$tileLayer(provider = "Stamen.TonerLite")
      temp <- comb[which(comb$County == input$county),]
      centroids <- coordinates(fl)[which(fl$County == input$county),]
      mymap$setView(rev(centroids), zoom = 10)
      #       # points
      #       temp <- comb[which(!is.na(comb$lat) & !is.na(comb$lon)),]
      #       temp2 <- temp
      #       coordinates(temp2) <- ~lon+lat
      #       proj4string(temp2) <- proj4string(fl)
      #       places <- over(temp2, polygons(fl) )
      #       x <- temp[which(places  == which(fl$County == "Alachua"))]
      
      
      mymap$enablePopover(TRUE)
      
      mymap$set(dom = 'myChart2')
      for (i in which(!is.na(temp$lat) & !is.na(temp$lon))){
        mylabel <- paste0(str_replace_all(temp$Name.of.Employer[i], "[^[:alnum:]]", " "),
                          " || ",
                          "Sector: ",
                          ifelse(is.na(temp[i,11]),
                                 "unknown",
                                 temp[i,11]),
                          " || ",
                          "Interviewed: ",
                          format(as.Date(temp$Date.of.Interview[i],
                                         format = "%m/%d/%Y"),
                                 "%b %d %Y"),
                          " || ",
                          "Number of employees: ",
                          as.character(temp$X..employees.at.work.place[i]))
        
        #print(mylabel)
        mymap$marker(c(temp$lat[i],
                       temp$lon[i]),
                     #bindPopup = "<p> Hi. I am a popup. </p>")}
                     bindPopup = mylabel)}
      
    }
    
    mymap
    
    
  })
  
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
             percent = TRUE, #input$percent,
             legend.cex = input$legend.cex, 
             legend.title = input$legend.title,
             err.cex = input$err.cex#,
             #ylab = input$ylab
             )
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
             percent = TRUE, #input$percent,
             legend.cex = input$legend.cex, 
             legend.title = input$legend.title,
             err.cex = input$err.cex#,
             #ylab = input$ylab
             )
      
    }
    
    })
  #############
  output$plot2 <- renderPlot({
   
    ChoroFun(var_index = as.numeric(input$xvar))
    
  })

#############
output$plot3 <- renderPlot({
  
  if(input$yvar == 0){
    plot(fl, col = adjustcolor("grey", alpha.f =0.4), 
         border = adjustcolor("black", alpha.f=0.2))
    text(x = -83, y = 29, cex = 2, labels = "Pick a 'cross variable'\n to see a second map down here.")
  } else{
    ChoroFun(var_index = as.numeric(input$yvar))
    
  }
  
})
  
  

})
