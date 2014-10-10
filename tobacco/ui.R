library(shiny)
suppressPackageStartupMessages(library(googleVis))

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Tobacco data explorer"),
  
  sidebarPanel( 
    tags$a(href="mailto:joseph.brew@flhealth.gov", "Joe Brew"                        
    
    #tags$img(src="https://docs.google.com/drawings/d/1z5OtlqZ8ex1QhHDtsCOvYMM2WgFsYgD1onjSi-Kus40/pub?w=534&h=89", height="100px"
    ),
    
    
    radioButtons("team","Team",c(
      "1"="1",
      "2"="2",
      "3"="3",
      "4"="4"))#,
    
    #     htmlOutput("selectUI")
    
    #     selectInput("grade", "Grade",c(
    #       "Pre-k"="-1",
    #       "K"="0",
    #       "1"="1",
    #       "2"="2",
    #       "3"="3",
    #       "4"="4",
    #       "5"="5",
    #       "5"="5",
    #       "6"="6",
    #       "7"="7",
    #       "8"="8",
    #       "9"="9",
    #       "10"="10",
    #       "11"="11",
    #       "12"="12"      
    #       ))
    
    #     sliderInput("year", "Year", 
    #                 min=2006, max=2013, value=2006, step=1,
    #                 animate=animationOptions(interval = 3000, loop = FALSE,
    #                                          playButton = "Play", pauseButton = "Pause"))
    
    #submitButton(text="Ready")
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Univariate visuals",
               #htmlOutput("motionchart1")
               plotOutput("plot1")
      ),
      
      tabPanel("Cross-tabulations",
               #h4("Immunization rate by year"),
               #h4(textOutput("text1"), align="center"),
               plotOutput("plot2")),
      #tabPanel("Details",htmlOutput("linech"),
      
      tabPanel("Map",
               #h4(textOutput("text2"), align="center"),
               plotOutput("plot3"))

      
      #,
      #tabPanel("Details",htmlOutput("linech")
    )
  )
))