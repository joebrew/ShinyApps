
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(weatherData)
library(ggplot2)
library(ggvis)
library(dplyr)

shinyServer(function(input, output) {
  
  weather <- reactive({
    
    if(input$place != 'GNV'){
    
      df <- getWeatherForDate(toupper(input$place), 
                              start_date = "2000-01-01", 
                              end_date="2000-12-31")
      years <- 2001:2014
      for (i in 1:length(years)){
        new_df <- getWeatherForDate(toupper(input$place), 
                                    start_date = paste0(years[i],"-01-01"), 
                                    end_date= paste0(years[i],"-12-31"))
        df <- rbind(df, new_df)
      }
      # Add 2015
      new_df <- getWeatherForDate(toupper(input$place), 
                                  start_date = paste0(years[i],"-01-01"), 
                                  end_date= Sys.Date())
      df <- rbind(df, new_df)
      df$date <- as.Date(df$Date)
      df  
      
    } else{
      df <- read.csv('gnv_weather.csv')
      df$date <- as.Date(df$Date)
      df
    }
  })

  output$distPlot <- renderPlot({

    g <- ggplot(data = weather())
    g + aes(x = Date, y = Max_TemperatureF) +
      geom_point(color = 'red', alpha = 0.5) +
      geom_point(aes(x = Date, y = Min_TemperatureF),
                 color = 'blue', alpha = 0.5)
    
    df %>%
      ggvis(~Date, ~Max_TemperatureF) %>%
      layer_points()
    
  })

})
