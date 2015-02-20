
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source('models.R')

shinyServer(function(input, output) {
  
  
  # MAKE CALCULATIONS
  n <- reactive(input$n)
  reactive(number_private <- round(input$n * p_private))
  reactive(number_nonprivate <- n - number_private)
  reactive(ir <- weighted.mean(c(input$irp, input$irnp), w = c(number_private, number_nonprivate)))
  
  reactive(nr <- net_rev(number_private = number_private,
                number_nonprivate = number_nonprivate,
                ir = ir,
                collaborative = input$collaborative,
                reim_np = input$reim_np,
                reim_p = input$reim_p,
                suc_np = input$suc_np,
                suc_p = input$suc_p))
  
  output$tx <- renderText({print(nr)})

  output$distPlot <- renderPlot({

    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')

  })
  
  output$test <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    barplot(1:input$irnp)
    text(x = 5, y = 5, labels = nr)
    
  })

})
