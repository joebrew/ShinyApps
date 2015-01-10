library(shiny)
library(knitr)

# Define UI for application
shinyUI(pageWithSidebar(
  
  ## Application title
  headerPanel("Eric's favorite color, number and fish!"),
  
  ## Sidebar panel
  sidebarPanel("This app generates a pdf report about some of Eric's favorite things.  
               The contents of this report are controlled by user input.",
               br(),
               
               "The source code is available on ",
               a(href = "https://github.com/joebrew/ShinyApps/tree/master/minimal",
                 "GitHub."),
               br(),
               downloadButton("downloadPDF", "Download your report, you asshole!")
  ),
  
  
  ## Main panel
  mainPanel(
    wellPanel(
      textInput("color",
                "Eric's favorite color:",
                "girly pink"),
      selectInput("fish", "Eric's favorite fish",
                  c("Goldfish" = "goldfish",
                    "Whale" = "whale",
                    "Catfish" = "catfish",
                    "Phish" = "phish")),
      sliderInput("number", "Eric's favorite number",
                  min=1, max=10, value=6, step=1)
  )
)
))

