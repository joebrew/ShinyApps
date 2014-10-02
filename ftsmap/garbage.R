library(rCharts)
library(shiny)
ui = (pageWithSidebar(
    headerPanel("Heatmap"),
    sidebarPanel( width=2),
    mainPanel(
      mapOutput("leafmap")
    )
  )),
  server = function(input, output) {
    output$leafmap  <- renderMap({
      L2 <- Leaflet$new()
      L2$setView(c(29.7632836,  -95.3632715), 10)
      L2$tileLayer(provider = "MapQuestOpen.OSM")
      data(crime, package = 'ggmap')
      library(plyr)
      crime_dat = ddply(crime, .(lat, lon), summarise, count = length(address))
      crime_dat = toJSONArray2(na.omit(crime_dat), json = F, names = F)
      L2$addAssets(jshead = c(
        "http://leaflet.github.io/Leaflet.heat/dist/leaflet-heat.js"
      ))
      L2$setTemplate(afterScript = sprintf("
                                           <script>
                                           var addressPoints = %s
                                           var heat = L.heatLayer(addressPoints).addTo(map)           
                                           </script>
                                           ", rjson::toJSON(crime_dat)
      ))
      
      L2
    })
  }