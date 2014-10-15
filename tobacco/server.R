
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

shinyServer(function(input, output) {


  ########### INTERACTIVE MAP
  output$myChart2 <- renderMap({
    mymap <- Leaflet$new()
    mymap$tileLayer(provider = "Stamen.TonerLite")
    mymap$setView(c(27.85, -81.3), zoom = 6)
    mymap$enablePopover(TRUE)
    #     mymap$marker(c(51.5, -0.09), bindPopup = "Hi. I am a popup")
    #     mymap$marker(c(51.495, -0.083), bindPopup = "Hi. I am another popup")
    #     
    mymap$set(dom = 'myChart2')
    for (i in which(!is.na(comb$lat & is.na(comb$lon)))){
      mylabel <- paste0(str_replace_all(comb$Name.of.Employer[i], "[^[:alnum:]]", " "),
                        " || ",
                        "Sector: ",
                        ifelse(is.na(comb[i,11]),
                                     "unknown",
                                     comb[i,11]),
                        " || ",
                        "Interviewed: ",
        format(as.Date(comb$Date.of.Interview[i],
                       format = "%m/%d/%Y"),
               "%b %d %Y"),
        " || ",
      "Number of employees: ",
      as.character(comb$X..employees.at.work.place[i]))

      #print(mylabel)
      mymap$marker(c(comb$lat[i],
                     comb$lon[i]),
      #bindPopup = "<p> Hi. I am a popup. </p>")}
      bindPopup = mylabel)}
#     ,
#                    bindPopup = paste0(comb$Name.of.Employer[i]))
#     }
#       )
#     }
#                    
                   
#                    ,
#                    bindPopup = paste0(as.character(comb$Name.of.Employer[i]),
#                                       " (interview date: ", 
#                                       ifelse(is.na(comb$Date.of.Interview[i]), 
#                                              "unknown",
#                                              format(as.Date(comb$Date.of.Interview[i],
#                                                             format = "%m/%d/%Y"),
#                                                     "%b %d %Y")),") ",
#                                       "Number of employees: ",
#                                       as.character(comb$X..employees.at.work.place[i])))
      
#    }
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
   
    mymap <- map("county", "florida")
    title(main = "Under construction")
    
  })
  
  

})
