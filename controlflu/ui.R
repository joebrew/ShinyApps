library(shiny)
suppressPackageStartupMessages(library(googleVis))

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Control Flu"),
  
  sidebarPanel( 
#     numericInput("minedad", "Minimum Age",-1,-1,99),
#     numericInput("maxedad", "Maximum Age",99,-1,99),
    #checkboxGroupInput
    radioButtons("team","Team",c(
      "1"="1",
      "2"="2",
      "3",
      "4"))#,
    
#     sliderInput("year", "Year", 
#                 min=2006, max=2013, value=2006, step=1,
#                 animate=animationOptions(interval = 3000, loop = FALSE,
#                                          playButton = "Play", pauseButton = "Pause"))

    #submitButton(text="Ready")
    ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Overview",
               h4("Immunization rate by year"),
               #htmlOutput("motionchart"),
               plotOutput("plot1"),
               textOutput("text1"))#,
      #tabPanel("Details",htmlOutput("linech"))
    )
  )
))