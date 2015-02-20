library(shiny)
suppressPackageStartupMessages(library(googleVis))


shinyServer(function (input, output#, 
                      #session
                      ) {
  
  source("helper.R")
  

  
  ########
  mydata <- reactive({
    
    if(input$type == "all"){
      ir
    } else {
      ir[which(ir$type == input$type),]
    }
    
    })
  
  #######

  ########
  
#   output$selectUI <- renderUI({ 
#     selectInput("team", "Select your school", mydata()$school)
#   })
#   
  #######
  output$plot1 <- renderPlot({

    barplot(1:10)
    })
  
  #######
  output$plot2 <- renderPlot({
    barplot(1:10)
  })
  
  #######
  output$plot3 <- renderPlot({
    barplot(1:30)
    
    })
  
  #######
  output$plot4 <- renderPlot({
    barplot(1:40)
    })
  
  #######
  output$plot5 <- renderPlot({
    
    plot(1:10, 11:20)
  })
  
  #######
  output$plot6 <- renderPlot({
    
    barplot(5:25)
  })
  
  #######
  output$plot7 <- renderPlot({
    barplot(1:10)
  }, height=1000)
  
  #######
  output$plot8 <- renderPlot({
    
    barplot(1:10)
    
  }, height = 1000)
  
  
  
  
  

  ########
  output$text1 <- renderText({
    paste("Team", input$type, "immunization rate overview")
  })
  
  ########
  output$text2 <- renderText({
    paste("Team", input$type, "consent form return rate overview")
  })
  

  ########
  output$table1 <- renderDataTable({
    
    mydata()
    
  })
  
  ########
  output$motionchart1 <- renderGvis({
    
    gvisMotionChart(data = mydata(), 
                    idvar = "school", 
                    timevar = "year",
                    xvar = "frLunch13",
                    yvar = "immRate",
                    colorvar = "gradeRange",
                    sizevar = "totMem")

  })
  
  

})