
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

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
      
      h6("Download month"),
      downloadButton('downloadData1', 'Download'),
      h6("Download all months"),
      downloadButton('downloadData2', 'Download')
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Monthly Overview",
          plotOutput("plot1"),
          dataTableOutput("table1")
          ),
        
        tabPanel("Monthly Details",
                 plotOutput("plot2"),
                 dataTableOutput("table2")
        ),
        
        tabPanel("All farms",
                 plotOutput("plot3")
        )
        
        

        )
      

    )
  )
))
