library(leaflet)
library(RColorBrewer)
library(maps)
library(ggplot2)
library(rgdal)
library(leafletR)
library(rgeos) #for simplification
library(sp)
library(ggmap)

################## NEW STUFF
rm(list = ls())


################## MY STUFF
rm(list = ls())

fl <- map("county","fl", plot=FALSE, fill=TRUE)
fl$names <- gsub(":|florida,|main", "", fl$names ) 
fl$names <- gsub("spit", "", fl$names)
fl_df <- fortify(fl, region = 'id')
each_county <- split(fl_df, fl_df$region)

shinyServer(function(input, output, session) {
  map <- createLeafletMap(session, "map")
  observe({if (is.null(input$map_click)) {
    return()
  }
  })
  
  # session$onFlushed is necessary to delay the drawing of the polygons until
  # after the map is created
  session$onFlushed(once= T, function() {    
    map$addPolygon(fl$y, fl$x, fl$names,
                   lapply(brewer.pal(7, "Greys"), function(x) {
                     list(fillColor = x)
                   }),
                   list(fill=T, fillColor="#800026", fillOpacity=.6, 
                        stroke=TRUE, opacity=1, color="white", weight=1.5, dashArray = '3' 
                   )
    ) 
  })
  
  observe({   
    # mouseover event (eventOver)
    # when the pointer is over a state, highlight that state and 
    # show information. layerID of addPolygon should be set as "sameID"
    # or any other name, so that the previou states are not highlighted.
    
    # when the highlighted state polygon is added, the eventOver$id will be 
    # updated to 's' (this is weird!). In order to avoid capturing the 
    # highlited state, set return() when eventOver$id == 's'. 
    eventOver <- input$map_shape_mouseover
    if (is.null(eventOver)||eventOver$id == 's') {
      return()
    }
    
    stateOver <- each_county[[eventOver$id]]
    map$addPolygon(stateOver$lat, stateOver$long, 'sameID') 
    output$eventOverInfo <- renderPrint({(eventOver$id)})    
  })
})



######### ORIGINAL TEMPLATE
# rm(list = ls())
# states <- map("state", plot=FALSE, fill=TRUE)
# states$names <- gsub(':', '.', states$names ) 
# statesDF <- fortify(states, region = 'id')
# eachState <- split(statesDF, statesDF$region)

# shinyServer(function(input, output, session) {
#   map <- createLeafletMap(session, "map")
#   observe({if (is.null(input$map_click)) {
#     return()
#   }
#   })
#   
#   # session$onFlushed is necessary to delay the drawing of the polygons until
#   # after the map is created
#   session$onFlushed(once= T, function() {    
#     map$addPolygon(states$y, states$x, states$names,
#                    lapply(brewer.pal(7, "Set1"), function(x) {
#                      list(fillColor = x)
#                    }),
#                    list(fill=T, fillColor="#800026", fillOpacity=.6, 
#                         stroke=TRUE, opacity=1, color="white", weight=1.5, dashArray = '3' 
#                    )
#     ) 
#   })
#   
#   observe({   
#     # mouseover event (eventOver)
#     # when the pointer is over a state, highlight that state and 
#     # show information. layerID of addPolygon should be set as "sameID"
#     # or any other name, so that the previou states are not highlighted.
#     
#     # when the highlighted state polygon is added, the eventOver$id will be 
#     # updated to 's' (this is weird!). In order to avoid capturing the 
#     # highlited state, set return() when eventOver$id == 's'. 
#     eventOver <- input$map_shape_mouseover
#     if (is.null(eventOver)||eventOver$id == 's') {
#       return()
#     }
#     
#     stateOver <- eachState[[eventOver$id]]
#     map$addPolygon(stateOver$lat, stateOver$long, 'sameID') 
#     output$eventOverInfo <- renderPrint({(eventOver$id)})    
#   })
# })

