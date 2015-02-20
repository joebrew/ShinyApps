
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("SLIV Sustainability Model"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      textInput("n",
                "Total number of students"),
      sliderInput('p_private',
                  'Percentage of students privately insured:',
                  min = 0, max = 100,
                  value = 40),
      sliderInput('irp',
                'Immunization rate (private):',
                min = 0, max = 100,
                value = 30),
      sliderInput('irnp',
                'Immunization rate (non-private):',min = 0, max = 100,
                value = 30),
      checkboxInput('collaborative',
                    "Collaborative cost structure?",
                    value = TRUE),
      textInput('reim_p',
                'Average reimbursement (private):',
                value = 47.05),
      textInput('reim_np',
                'Average reimbursement (non-private):',
                value = 5),
      sliderInput('suc_p',
        'Successful billing rate (private):',
                  value = 80, min = 0, max = 100),
      sliderInput('suc_np',
                  "Successful billing rate (non-private):",
                  value = 65, min = 0, max = 100)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      plotOutput('test'),
      textOutput('tx')
    )
  )
))
