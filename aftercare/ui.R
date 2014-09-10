
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("After care options"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("teacher",
                  "Teacher:",
                  min = 10,
                  max = 30,
                  value = 12),
      
      sliderInput("student",
                  "Student:",
                  min = 5,
                  max = 10,
                  value = 7)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Table",
                 h3("Cost/profit for school"),
                 tableOutput("table1")
        ),
        
      tabPanel("Visual",
               h3("Cost/profit for school"),
               plotOutput("plot1")
               )
)
    )
  )
))
