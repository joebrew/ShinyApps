
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Budget calculator"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      
      textInput("b4",
                "Total budget:",
                value = "300000"),
      
      sliderInput("b5",
                  "Number of messages:",
                  min = 1,
                  max = 10,
                  value = 1),
      
      sliderInput("b10",
                  "Cost of EIP mail per target:",
                  min = 0.00,
                  max = 5.00,
                  value = 2.00, 
                  step=0.10),
      
      sliderInput("b11",
                  "Cost of post-EIP mail per target:",
                  min = 0.00,
                  max = 5.00,
                  value = 1.50, 
                  step=0.10),
      
      sliderInput("b13",
                  "Phone survey contact rate:",
                  min = 0,
                  max = 100,
                  value = 10, 
                  step=1),
      
      textInput("b14",
                "Number of interviews per condition:",
                value = 2000),
      
      sliderInput("b15",
                  "Cost of phone survey per complete:",
                  min = 0,
                  max = 10,
                  value = 4.00, 
                  step=0.1),
      
      sliderInput("b18",
                  "Persuasive effect of mail without EIP:",
                  min = 0,
                  max = 10,
                  value = 3.5, 
                  step=0.1),
      
      sliderInput("b19",
                  "Persuasive effect of mail with targeting EIP (one treatment):",
                  min = 0,
                  max = 20,
                  value = 10, 
                  step=0.1),
      
      sliderInput("b20",
                  "Persuasive effect of mail with targeting EIP (2 or more treatments):",
                  min = 0,
                  max = 25,
                  value = 13, 
                  step=0.1)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("a", width = "80%"),
      tableOutput("b")
    )
  )
))
