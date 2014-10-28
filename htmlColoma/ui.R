# Define UI for slider demo application
shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel("HTML training"),
  

  sidebarPanel(
    
    helpText("Make some choices:"),



    # HOURS PER DAY
    sliderInput("perDay", "Goal hours per day", 
                min=0, max=10, value=1),
    
    # TOTAL GOAL HOURS
    sliderInput("tot.goal", "Goal cumulative hours", 
                min=0, max=1000, value=500, step=50),
    
    # WEEKEND WORK
    selectInput("weekend", "Do your hours per day include weekends?",
    c("Yes" = "TRUE",
      "No" = "FALSE"))

  ),
  
  mainPanel(
    h4(
    textOutput("text1")
    ),
    
    plotOutput("plot1")

  )
))