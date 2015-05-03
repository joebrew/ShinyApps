
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
                  'DEVONEY',
                  'DIGBY',
                  'EILIN',
                  'ELEANOR',
                  'EVELYN',
                  'GRADY',
                  'HUDSON',
                  'JAELYN',
                  'JOJO',
                  'JONAS',
                  'KASEY',
                  'KATE',
                  'LAUTARO',
                  'LAYLA',
                  'LEILA',
                  'MAXWELL',
                  'NAOMI GOBLE',
                  'NATHAN GOBLE',
                  'OWEN H',
                  'OWEN R',
                  'PHETHRA',
                  'QUENTIN',
                  'RILEY',
                  'SAWYER',
                  'SIMINI',
                  'TALEA',
                  'TAYEKO',
                  'TOMMY',
                  'ZOLTAHN')),
      checkboxInput("show_vals",
                    "Show dollar amount",
                    value = TRUE),
      checkboxInput("show_payments",
                    "Show Payments",
                    value = FALSE),
      br(), br(), 
      
      downloadButton("downloadPDF", "Download individual report"),
      br(), br(), 
      downloadButton("downloadPDF_master", "Download master report")
      
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      dataTableOutput("table")
    )
  )
))
