library(shiny)
library(rCharts)

################################

## app.R
server <- function(input, output) {
  require(shiny)
  require(rCharts)
  shinyServer(function(input, output){
    output$myChart2 <- renderMap({
      map3 <- Leaflet$new()
      map3$setView(c(51.505, -0.09), zoom = 13)
      map3$marker(c(51.5, -0.09), bindPopup = "Hi. I am a popup")
      map3$marker(c(51.495, -0.083), bindPopup = "Hi. I am another popup")
      map3$set(dom = 'myChart2')
      map3
    })
  })
}

#ui 

# ui <- shinyUI(fluidPage(
#   #sidebarLayout("test"),
#   #sidebarPanel("test"),
#   mainPanel("map", tags$style('.leaflet {height: 400px;}'),showOutput('myChart2', 'leaflet'))
# #    tabsetPanel(
# #      tabPanel(
#        
# #        )
# #    )
# #  )
# ))

ui <- shinyUI(pageWithSidebar(
  headerPanel("test"),
  sidebarPanel(),
  mainPanel(
    tabsetPanel(
      tabPanel("map", tags$style('.leaflet {height: 400px;}'),showOutput('myChart2', 'leaflet'))
    )
  )
))

shinyApp(ui = ui, server = server)
