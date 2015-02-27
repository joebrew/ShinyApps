
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source('non_prediction_simulation.R')

shinyServer(function(input, output) {

  output$plot <- renderPlot({
    x <- portfolio_value(data = df, model = fit, n = 1000)
    
    # Original distribution
    hist(x, freq = FALSE, breaks = 50,  border = NA,
         xlim = c(43000, 57000),
         main = "Possible bootsrapped portfolio values",
         xlab = "Dollars",
         col = adjustcolor('blue', alpha.f = 0.2))
    
    # Bootstrapped distributions at varying number of bootstraps
    ns <- round(seq(50,1000, length = 10))
    cols <- colorRampPalette(c('blue', 'lightblue'))(10)
    for (i in 1:length(ns)){
      x <- portfolio_value(data = df, model = fit, n = ns[i])
      lines(density(x), col = adjustcolor(cols[i], alpha.f = 0.5))
      #Sys.sleep(0.1)
    }
    # Note: distribution remains unchanged by n bootstraps
    
    # Add 2.5 and 97.5 lines
    abline( v= quantile(x, probs = c(0.025, 0.5, 0.975)),
           col = adjustcolor("blue", alpha.f = 0.4), lwd = c(1,3,1),
           lty = c(3,1,3))
    
    if(input$show_new){
      new_df <- change_female(data = df, p_female = input$female)
      
      y <- portfolio_value(data = new_df, model = fit, n = 1000)
      
      # New distribution
      hist(y, freq = FALSE, breaks = 50,  border = NA,
           add = TRUE,
           col = adjustcolor('red', alpha.f = 0.2))
      
      ####
      # PREDICT AND RESAMPLE ON *NEW* DATA
      # USING ORIGINAL LINEAR MODEL
      #####
      ns <- round(seq(50,1000, length = 10))
      cols <- colorRampPalette(c('red', 'darkred'))(10)
      for (i in 1:length(ns)){
        z <- portfolio_value(data = new_df, model = fit, n = ns[i])
        lines(density(z), col = adjustcolor(cols[i], alpha.f = 0.5))
        #Sys.sleep(0.5)
      }
      
      # Add 2.5 and 97.5 lines
      abline( v= quantile(y, probs = c(0.025, 0.5, 0.975)),
              col = adjustcolor("red", alpha.f = 0.4), 
              lwd = c(1,3,1),
              lty = c(3, 1, 3))
      
      # Add text
      legend('topleft',
           col = adjustcolor(c("blue","white",  "red", "white"), alpha.f = 0.4),
           legend = c(paste0("Original: $",  round(median(x))),
                      paste0("(", round(quantile(x, probs = 0.025)),
                             "-", round(quantile(x, probs = 0.975)), ")"),
                      paste0("Modified: $", round(median(y))),
                      paste0("(", round(quantile(y, probs = 0.025)),
                             "-", round(quantile(y, probs = 0.975)), ")")),
           title = "Predicted portfolio value",
           lty = 1)
      
      legend('topright',
             border = NA,
             fill = adjustcolor(c('blue', 'red'), alpha.f = 0.6),
             legend = c('Original distribution', 'Modified distribution'))
      
    }

    

  })
  
  output$text <- renderText({
    paste(summary(fit)['call'])
    
  })
  
  output$table <- renderTable({
    coefs <- data.frame(summary(fit)['coefficients'])
    names(coefs) <- c('Coefficient', 'Std. Error', 'T-value', 'P-value')
    rownames(coefs) <- c('Intercept', 'x1 (continuous predictor)',
                         'Being male (categorical predictor)')
    coefs
  })

})
