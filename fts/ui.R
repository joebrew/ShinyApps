
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
      selectInput("month", "Month",c(
        "January", "February", "March", "April",
        "May", "June", "July", "August",
        "September", "October", "November", "Decmber"), selected="August"),
      
      h6("Download report (one month only)"),
      downloadButton('downloadData1', 'Download'),
      h6("Download report (all months)"),
      downloadButton('downloadData2', 'Download')
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        
#         tabPanel("Monthly Details",
#                  dataTableOutput("table2")
#         ),
#         
        tabPanel("Interactive farm map (new!)", tags$style('.leaflet {height: 400px;}'),showOutput('myChart2', 'leaflet')),
        
        tabPanel("Interactive school map (new!)", tags$style('.leaflet {height: 400px;}'),showOutput('myChart3', 'leaflet')),
        
        tabPanel("Monthly Overview",
                 plotOutput("plot1"),
                 dataTableOutput("table1")
        ),
        
#         tabPanel("Under construction",
#                  mapOutput("map_container")
#         ),
        
        tabPanel("All farms",
                 plotOutput("plot3"),
                 br(),
                 br(),
                 br(),
                 br(),
                 dataTableOutput("table3")
        )
        
        

        )
      

    )
  )
))
