# library("rCharts")
# library("shiny")
# 
# # Define UI for miles per gallon application
# shinyUI(pageWithSidebar(
#   
#   # Application title
#   headerPanel("Miles Per Gallon"),
#   
#   sidebarPanel(),
#   
#   mainPanel(
#     mapOutput("leafmap"))
# ))
# 
# # 
# # 
# # shinyUI(pageWithSidebar(
# #   headerPanel("Heatmap"),
# #   #sidebarPanel(width=2),
# #   mainPanel(
# #     mapOutput("leafmap")
# #   )
# # ))


library(shiny);library(rCharts)
shinyUI(navbarPage("rMaps Leaflet Sizing",
                   tabPanel("Map",
                            #  tags$style('.leaflet {height: 100%; width: 100%}'), --no change 
                            mapOutput('mapPlotJSON')
                   )
))
