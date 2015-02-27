
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Non-predictive simulation example"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      helpText("In our original data, about 50% are females. Pick a different % female for our new data."),
      sliderInput("female",
                  "Percent female",
                  min = 1,
                  max = 100,
                  value = 50),
      helpText('Now, click below to see the portfolio value of our new data.'),
      checkboxInput('show_new',
                    'Show new distribution',
                    value = FALSE),
      helpText('Slide the % female up or down to see how it changes')
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot"),
      h4('Model formula:'),
      textOutput('text'),
      h4('Model details'),
      tableOutput('table')
    )
  )
))
