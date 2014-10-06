# ui.R
require(shiny)
require(rCharts)
shinyUI(pageWithSidebar(
  headerPanel("test"),
  sidebarPanel(),
  mainPanel(
    tabsetPanel(
      tabPanel("map", tags$style('.leaflet {height: 400px;}'),showOutput('myChart2', 'leaflet'))
    )
  )
))

