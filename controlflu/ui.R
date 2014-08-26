library(shiny)
suppressPackageStartupMessages(library(googleVis))

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Control Flu Data Explorer"),
  
    sidebarPanel( 
      tags$img(src="https://docs.google.com/drawings/d/1z5OtlqZ8ex1QhHDtsCOvYMM2WgFsYgD1onjSi-Kus40/pub?w=534&h=89", height="100px"),
    

    radioButtons("team","Team",c(
      "1"="1",
      "2"="2",
      "3"="3",
      "4"="4")),
    
    selectInput("grade", "Grade",c(
      "Pre-k"="-1",
      "K"="0",
      "1"="1",
      "2"="2",
      "3"="3",
      "4"="4",
      "5"="5",
      "5"="5",
      "6"="6",
      "7"="7",
      "8"="8",
      "9"="9",
      "10"="10",
      "11"="11",
      "12"="12"      
      ))
    
#     sliderInput("year", "Year", 
#                 min=2006, max=2013, value=2006, step=1,
#                 animate=animationOptions(interval = 3000, loop = FALSE,
#                                          playButton = "Play", pauseButton = "Pause"))

    #submitButton(text="Ready")
    ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Explorer",
               htmlOutput("motionchart1")               
               ),
      
      tabPanel("Immunization Rate",
               #h4("Immunization rate by year"),
               h4(textOutput("text1"), align="center"),
               plotOutput("plot1"),
               plotOutput("plot2")),
      #tabPanel("Details",htmlOutput("linech"),
      
      tabPanel("Consent Form Return",
               h4(textOutput("text2"), align="center"),
               plotOutput("plot3"),
               plotOutput("plot4")),
      
      tabPanel("Raw data",
               h4("All years"),
               dataTableOutput("table1")),
      
      tabPanel("All teams",
               htmlOutput("motionchart2")),
      tabPanel("Expected vs. observed",
               h4("Under construction"),
               paste("This section will give an overview of each school's expected vs. observed",
                     "immunization and consent form return rates, after statistical adjustment for factors like",
                     "age of student population, poverty, and funding source (public vs. private).  The purpose",
                     "is to identify schools which are performing significantly better or worse than expected,",
                     "so as to either copy successful strategies or target specific interventions for improvement."),
               h2("Coming on or before August 28.")
               )
      
      #,
      #tabPanel("Details",htmlOutput("linech")
      )
    )
))