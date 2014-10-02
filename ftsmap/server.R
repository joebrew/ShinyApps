# library("rCharts")
# library("shiny")
# 
# shinyServer(function(input, output) {
#   output$leafmap  <- renderMap({
#     mymap <- Leaflet$new()
#     mymap$tileLayer(provider = "Stamen.TonerLite")
#     mymap$setView(c(29.65, -82.3), zoom = 7)
#     
#     mymap
#     
#   })
# })
# 
# 
# require(shiny)
# require(rCharts)
# shinyServer(function(input, output, session){
#   output$map_container <- renderMap({
#     plotMap(input$network)
# #   })
# })


library(shiny);library(rCharts)
shinyServer(function(input, output, session) {
  output$mapPlotJSON <- renderMap({
    map1 = Leaflet$new()
    map1$setView(c(45.5236, -122.675), 13)
    map1$tileLayer(provider ='Stamen.Terrain')
    #   map1$set(width = "100%", height = "100%") --doesn't show map
    map1
  })
})