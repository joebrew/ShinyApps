
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

source('birth.R')

shinyServer(function(input, output){
  
  date <- reactive({ 
    as.Date(input$date)})
  
  due_date <- reactive({
    as.Date(input$due_date)})
  
  temp <- reactive({
    how(date = date(),
        due_date = due_date())
    })
    

   
  output$plot1 <- renderPlot({
    

    plot(temp()$date, temp()$cum_p,
         type = 'l',
         col = adjustcolor('black', alpha.f = 0.6),
         lwd = 3, 
         xaxt = 'n',
         xlab = NA,
         ylab = 'Cumulative probability')
    axis(side = 1,
         at = temp()$date,
         labels = temp()$date,
         las = 3,
         cex.axis = 0.7)
    abline(h = seq(0, 100, 20),
           col = adjustcolor('black', alpha.f = 0.4))
    abline(v = temp()$date, 
           col = adjustcolor('black', alpha.f = 0.6))
    abline(h = 50, col = adjustcolor('darkred', alpha.f = 0.6), lwd = 2)
    title(main = 'Cumulative probability by day\n(ie, will you have given birth by each day)')
        
  })
  
  output$plot2 <- renderPlot({
    plot(temp()$date, 
         temp()$percentage,
         type = 'l', 
         col = adjustcolor('black', alpha.f = 0.6),
         lwd = 3,
         xaxt = 'n',
         xlab = NA,
         ylab = 'Daily probability')
    axis(side = 1,
         at = temp()$date,
         labels = temp()$date,
         las = 3,
         cex.axis = 0.7)
    abline(h = seq(0, 100, 2),
           col = adjustcolor('black', alpha.f = 0.4))
    abline(v = temp()$date, 
           col = adjustcolor('black', alpha.f = 0.6))
    title(main = 'Individual day-specific probabilities (for gambling purposes')
  })
#   
#   output$plot3 <- renderPlot({
#     # Cascading probabilities
#     dates <- seq(Sys.Date() - 30, Sys.Date() + 15, 1)
#     
#     plot(dates, seq(1, 100, length = length(dates)),
#          type = 'n', xlab = 'Date',
#          ylab = 'Cumulative probability of spontaneous labor',
#          cex.axis = 0.7)
#     for (i in 1:length(dates)){
#       temp_date <- dates[i]
#       if(temp_date == Sys.Date()){
#         col <- 'red'
#         lwd <- 3
#       } else{
#         col <- 'black'
#         lwd <- 1
#       }
#       col <- adjustcolor(col, alpha.f = 0.5)
#       temp_df <- how(date = as.Date(temp_date),
#                      due_date = input$due_date)
#       lines(temp_df$date, temp_df$cum_p, 
#             col = col,
#             lwd = lwd)
#     }
#     abline(v = as.Date('2015-05-21'), 
#            col = adjustcolor('darkblue', alpha.f = 0.6))
#     
#     legend('topleft',
#            lty = 1,
#            legend = c('Bottom of line = date you\'re at now',
#                       'Rest of line = your trajectory'),
#            col = 'white')
#     title(main = 'When will Coloma give birth?')
#   })
#   
  output$table1 <- renderTable({
    x <- temp()
    x$date <- as.character(x$date)
    names(x) <- c('Date', 'Week', 'Day', 'Day probability', 'Cumulative probability')
    x
  })
  
  output$text1 <- renderText({
    x <- temp()
    paste0('As of today, greater than 50% chance that the baby will be here by ',
           format(as.Date(x$date[which(x$cum_p == min(x$cum_p[which(x$cum_p >= 50)]))]),
           '%B %d, %Y'),
           '\n... but, if you\'re a betting man, the best day to pick is ',
           format(as.Date(x$date[which(x$percentage == max(x$percentage))]),
                  '%B %d, %Y'))
  })
  
})
  

