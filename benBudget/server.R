library(shiny)

#source the budgetFun file
source("~/ShinyApps/benBudget/helper.R")



# Define server logic for slider examples
shinyServer(function(input, output) {
  
  output$text1 <- renderText({ 
    
    paste(ifelse(input$euro, "€", "$") , 
    
    BudgetFun(tu = input$tu,
              rent = input$rent,
              flight= input$flight,
              christmas= input$christmas,
              euro= input$euro,
              food= input$food,
              other= input$other)
    )
    
    })
  
  output$plot1 <- renderPlot({
    
    PlotBudgetFun(tu = input$tu,
                  rent = input$rent,
                  flight= input$flight,
                  christmas= input$christmas,
                  food= input$food,
                  other= input$other)
    
    
  })

})