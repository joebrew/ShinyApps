library(shiny)
suppressPackageStartupMessages(library(googleVis))

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Control Flu data explorer"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.tabs == 'Explorer'",
      
        radioButtons("type","Age group",c(
          "all" = "all",
          #"prek" = "prek",
          "elem"="elem",
          "mid"="mid",
          "high"="high"
          #"multi"="multi"
          ))
        ),
      
      conditionalPanel(
        condition = "input.tabs == 'Absenteeism rate'",
        
        sliderInput("ir","Immunization rate (we're at 42%)",
                    min = 0,
                    max = 100,
                    value = 15),
        sliderInput("flu_days","Length of flu season",
                    min = 0,
                    max = 180,
                    value = 48)
      ),
      
      
      conditionalPanel(
        condition = "input.tabs == 'Raw data'",
        
        textInput("filter","Search")
      )
  
    ),
  
  mainPanel(
    tabsetPanel(id = "tabs",
      tabPanel("Explorer",
               htmlOutput("motionchart1")               
               ),
      
      tabPanel("Absenteeism rate",
               h4("Predicted absenteeism rates (average year)", align="center"),
               plotOutput("plot3"),
               plotOutput("plot2")
               #plotOutput("plot1")
               ),
      

      tabPanel("Raw data",
               plotOutput("plot4"),
               h4("All years"),
               dataTableOutput("table1"))
      
      #,
      #tabPanel("Details",htmlOutput("linech")
      )
    )
  )
))