# Define UI for slider demo application
shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel("Budget"),
  

  sidebarPanel(
    
    helpText("Estimate the below expenses", em("in euros.")),

    # RENT
    sliderInput("tu", "Tuition:", 
                min=0, max=6000, value=500, step=50),
    
    # RENT
    sliderInput("rent", "Rent (your share, including utilities):", 
                min=0, max=1000, value=500, step=10),
    
    # FLIGHT
    sliderInput("flight", "Getting there and back:", 
                min=500, max=1500, value=1000, step=50),
    
    # CHRISTMAS FLIGHT
    sliderInput("christmas", "Flight home for Christmas:", 
                min=0, max=1500, value=1000, step=50),
    
    # FOOD PER DAY
    sliderInput("food", "Food per day", 
                min=0, max=20, value=10),
    
    
    # OTHER PER DAY
    sliderInput("other", "Other expenses per day", 
                min=0, max=20, value=10),
    
    # EUROS
    selectInput("euro", "Show output in Euros or dollars:",
    c("Euros" = "TRUE",
      "Dollars" = "FALSE"))

  ),
  
  mainPanel(
    h2(
    textOutput("text1")
    ),
    h4("(for the year)"),
    plotOutput("plot1")

  )
))