
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("After care"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("student",
                  "Student:",
                  c('AMMALIA',
                  'AUDREY',
                  'AUGUSTUS',
                  'AVI',
                  'BENNETT',
                  'BRADEN',
                  'CASEY',
                  'DEVONEY',
                  'DIGBY',
                  'EILIN',
                  'ELEANOR',
                  'GRADY',
                  'HUDSON',
                  'JAELYN',
                  'JOJO',
                  'JONAS',
                  'KATE',
                  'LAUTARO',
                  'LAYLA',
                  'LEILA',
                  'MAX',
                  'NAOMI GOBLE',
                  'NATHAN GOBLE',
                  'OWEN H',
                  'OWEN R',
                  'PHETHRA',
                  'QUENTIN',
                  'SAWYER',
                  'SAWYER ',
                  'SIMINI',
                  'TALEA',
                  'TAYEKO',
                  'TOMMY',
                  'ZOLTHAN')),
      checkboxInput("show_vals",
                    "Show dollar amount",
                    value = FALSE)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      tableOutput("table")
    )
  )
))
