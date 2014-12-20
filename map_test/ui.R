shinyUI(fluidPage(
  textOutput('eventOverInfo'),
  
  # create a div to contain leaflet Map, so that I can use percentage to 
  # control the heigth of the map
  # or else the heigth must be absolute px value (e.g. 500)
  div(class = 'mapContainer',
      leafletMap(
        "map", width = "100%", height = "100%",
#         initialTileLayer = "http://{s}.tiles.mapbox.com/v3/jianhua1122.j5f28jda
#       /{z}/{x}/{y}.png",
#         initialTileLayerAttribution = NULL,
        options=list(
          center = c(27, -81),
          zoom = 8)
      )    
  ),
  
  tags$head(tags$style("
    .mapContainer {
      position: fixed;
      top: 50px;
      left: 0;
      right: 0;
      bottom: 0;
      overflow: hidden;
      padding: 0;
    }
  "))     
))
