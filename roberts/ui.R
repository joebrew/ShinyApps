library(shiny)
suppressPackageStartupMessages(library(googleVis))

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Control Flu Data Explorer"),
  
    sidebarPanel( 
      tags$img(src="https://docs.google.com/drawings/d/1z5OtlqZ8ex1QhHDtsCOvYMM2WgFsYgD1onjSi-Kus40/pub?w=534&h=89", height="50px"),
    

    radioButtons("type","Age group",c(
      "all" = "all",
      "prek" = "prek",
      "elem"="elem",
      "mid"="mid",
      "high"="high",
      "multi"="multi"))#,
    ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Explorer",
               htmlOutput("motionchart1")               
               ),
      
      tabPanel("Immunization Rate",
               h4(textOutput("text1"), align="center"),
               plotOutput("plot1")),
      
      tabPanel("Absenteeism Rate",
               h4(textOutput("text1"), align="center"),
               plotOutput("plot2")),
      

      tabPanel("Raw data",
               h4("All years"),
               dataTableOutput("table1"))
      
      #,
      #tabPanel("Details",htmlOutput("linech")
      )
    )
))