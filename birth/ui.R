
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("When will Anna give birth?"),
  
  # Sidebar with a slider input for number of bins
  sidebarPanel(
    textInput("date",
              "Today's date (YYYY-mm-dd)",
              value = Sys.Date()),
    textInput('due_date',
              label = 'Due date',
              value = '2015-06-23'),
    textOutput('text1')
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("plot1"),
    plotOutput("plot2"),
    #plotOutput("plot3"),
    tableOutput('table1')
  )
))
