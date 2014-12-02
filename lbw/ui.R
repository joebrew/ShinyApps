library(shiny)
suppressPackageStartupMessages(library(googleVis))


shinyUI(fluidPage(
  titlePanel("Counties explorer"),
  
  sidebarLayout(
    sidebarPanel( "Check it"),
    mainPanel(
      htmlOutput("motionchart1"))
    
  )
))


