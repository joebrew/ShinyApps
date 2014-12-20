
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(rCharts)

shinyUI(fluidPage(

  # Application title
  titlePanel("F2s"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      
      selectInput("county", 
                  label = "Choose a county",
                  choices = c("ENTIRE STATE", "ALACHUA","BAKER","BAY","BRADFORD",
                              "BREVARD","BROWARD","CALHOUN","CHARLOTTE",
                              "CITRUS","CLAY","COLLIER","COLUMBIA",
                              "DESOTO","DIXIE","DUVAL",
                              "ESCAMBIA",
                              "FLAGLER","FRANKLIN",
                              "GADSDEN","GILCHRIST",
                              "GLADES","GULF","HAMILTON","HARDEE",
                              "HENDRY","HERNANDO","HIGHLANDS","HILLSBOROUGH",
                              "HOLMES","INDIAN RIVER","JACKSON","JEFFERSON",
                              "LAFAYETTE","LAKE","LEE","LEON","LEVY",
                              "LIBERTY","MADISON","MANATEE","MARION",
                              "MARTIN","MIAMI-DADE","MONROE","NASSAU",
                              "OKALOOSA","OKEECHOBEE","ORANGE","OSCEOLA",
                              "PALM BEACH","PASCO","PINELLAS","POLK",
                              "PUTNAM","SANTA ROSA","SARASOTA","SEMINOLE",
                              "ST. JOHNS","ST. LUCIE","SUMTER","SUWANNEE",
                              "TAYLOR","UNION","VOLUSIA",
                              "WAKULLA","WALTON","WASHINGTON"),
                  selected = "ENTIRE STATE"),
      
      # IMMUNIZATION RATE
      sliderInput("ir", 
                  label=strong("Projected immunization rate:"), 
                  min=0, max=100, value=30),
      helpText("(usually 10-60%)", align="center"),
      br(),
      

      h6("Download county report"),
      downloadButton('downloadData1', 'Download')
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
             
        tabPanel("Map 1", tags$style('.leaflet {height: 400px;}'),showOutput('myChart1', 'leaflet')),
        
        tabPanel("Map 2", tags$style('.leaflet {height: 400px;}'),showOutput('myChart2', 'leaflet')),
        
        #tabPanel("Map 3", tags$style('.leaflet {height: 400px;}'),showOutput('myChart3', 'leaflet'))
        tabPanel("Map 3", mapOutput('myChart3'))

        )
      )
  )
))
