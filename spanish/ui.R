
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Steven and Michelle's Verb-o-rometer"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("verb_type",
                  "Verb type:",
                  c("Regular" = "regular",
                  "Irregular" = "irregular",
                  "Both" = "both"),
                  selected = "both")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tableOutput("table1")
    )
  )
))
