# server.R (from above, credit @ramnathv)
require(shiny)
require(rCharts)
shinyServer(function(input, output){
  output$myChart2 <- renderMap({
    mymap <- Leaflet$new()
    mymap$tileLayer(provider = "Stamen.TonerLite")
    mymap$setView(c(27.85, -81.3), zoom = 6)
    mymap$enablePopover(TRUE)
    mymap$marker(c(51.5, -0.09), bindPopup = "Hi. I am a popup")
    mymap$marker(c(51.495, -0.083), bindPopup = "Hi. I am another popup")
    


    
    mymap$set(dom = 'myChart2')
    mymap
  })
})