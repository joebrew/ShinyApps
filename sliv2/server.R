#####
# ATTACH LIBRARIES (some of these need to be installed from github, jcheng, ramnathv, etc.)
#####
library(leaflet)
library(RColorBrewer)
library(maps)
library(ggplot2)
library(rgdal)
library(leafletR)
library(rgeos) #for simplification
library(sp)
library(ggmap)
require(shiny)
require(rCharts)
library(leafletR)

#####
# READ IN GEOCODED SCHOOL DATA
#####
schools <- read.csv("schools.csv")

shinyServer(function(input, output, session) {
    
#   ########
#   mydata <- reactive({
#     farm[which(farm$month == input$month),]
#   })

  
  ############
  # LEAFLET MAP 1
  
  output$myChart1 <- renderMap({
    mymap <- Leaflet$new()
    mymap$tileLayer(provider = "Stamen.TonerLite")
    mymap$setView(c(27.85, -81.3), zoom = 6)
    mymap$enablePopover(TRUE)
#     mymap$marker(c(51.5, -0.09), bindPopup = "Hi. I am a popup")
#     mymap$marker(c(51.495, -0.083), bindPopup = "Hi. I am another popup")
#     
    mymap$set(dom = 'myChart2')
    
mymap
  })

############
# LEAFLET MAP 2

output$myChart2 <- renderMap({
  mymap <- Leaflet$new()
  mymap$tileLayer(provider = "Stamen.TonerLite")
  mymap$setView(c(27.85, -81.3), zoom = 6)
  mymap$enablePopover(TRUE)
  #     mymap$marker(c(51.5, -0.09), bindPopup = "Hi. I am a popup")
  #     mymap$marker(c(51.495, -0.083), bindPopup = "Hi. I am another popup")
  #     
  mymap$set(dom = 'myChart3')
  for (i in 1:50){
    mymap$marker(c(schools$LATITUDE[i],
                   schools$LONGITUDE[i]),
                 bindPopup = paste0(as.character(schools$SCHOOL_NAME_SHORT[i]),
                                    " (email: ", 
                                    ifelse(is.na(schools$EMAIL_ADDRESS_PRI[i]), 
                                           "unknown",
                                           as.character(schools$EMAIL_ADDRESS_PRI[i])),")"))
    
  }
  mymap
})
  

############
# LEAFLET MAP 3

#####
# DIRECTORIES
#####
root <- getwd()
setwd(root)

#####
# READ AND TRANSFORM FLORIDA CHOROPLETH
#####

# Read in map
fl <- readOGR("counties", "FCTY2")

# convert to lat lon
fl <- spTransform(fl, CRS("+init=epsg:4326"))  

# get x and y centroids
fl$x_centroid <- coordinates(fl)[,1]
fl$y_centroid <- coordinates(fl)[,2]

# create dataframe version of polygon data
fl_df <- fortify(fl, region = "NAME")
each_county <- split(fl_df, fl_df$group)

#####
# ASSIGN THE VARIABLE WE WANT TO MAP
#####
fl$var <- fl@data[,"N3"]

#####
# SETUP SOME PARAMETERS FOR WHERE TO READ AND WRITE GEOJSON FILES
# AND DO
#####

download_dir<-paste0(getwd(), "/counties")
file_name <- list.files(download_dir, pattern=".shp", full.names=FALSE)
file_name <- gsub(".shp", "", file_name)

www_dir <- paste0(getwd(), "/www")

#  Write data to GeoJSON
leafdat <- paste(www_dir, "/", file_name, ".geojson", sep="") 
zipgj <- toGeoJSON(data = fl, dest = paste0(getwd(),"/www"))

#####
# ESTABLISH CHOROPLETH PARAMETERS
#####
#  Create the cuts
cuts <- round(quantile(fl$var, probs = seq(0, 1, 0.20), na.rm = FALSE), 0)
cuts[1] <- 0 #  for this example make first cut zero

#  Fields to include in the popup
popup <- c("NAME", "var", "x_centroid", "y_centroid")

#  Gradulated style based on an attribute
sty <- styleGrad(prop="var", breaks=cuts, right=FALSE, style.par="col",
                 style.val=brewer.pal(5, "Reds"), leg="var", lwd=1)
#####
# GENERATE HTML FILE WITH LEAFLET MAP
#####
leaflet(data=zipgj, dest=www_dir, style=sty,
        title="index", base.map=c("mqsat", "osm", "tls", "mqosm", "toner", "water"), 
        incl.data=TRUE,  popup=popup)

# Define path to html file
addResourcePath("library", paste0(root, "/www/index"))

# Render html leaflet map into iframe
output$help <- renderUI({
  tags$script("library/index.html")
#   tags$iframe(
#     seamless="seamless",
#     #src = "https://localhost://www/index/index.html", # doesn't work
#     src="library/index.html",
#     height = "600",
#     width = "100%")
})



output$downloadData1 <- downloadHandler(
  filename = function() { paste(input$county, '.csv', sep='') },
  content = function(file) {
    write.csv(mydata_goodcolumns(), file)
  }
)


})
