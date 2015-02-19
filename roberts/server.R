library(shiny)
suppressPackageStartupMessages(library(googleVis))


shinyServer(function (input, output#, 
                      #session
                      ) {
  
  source("helper.R")
  

  
  ########
  mydata <- reactive({
    ir[which(ir$team2014 == as.numeric(as.character(input$team))),]
    })
  
  #######
  mydata13 <- reactive({
    mydata()[which(mydata()$year == 2013),]
    })
  
  ########
  
#   output$selectUI <- renderUI({ 
#     selectInput("team", "Select your school", mydata()$school)
#   })
#   
  #######
  output$plot1 <- renderPlot({
    TeamFun(team = as.numeric(input$team), data = mydata(), bar=FALSE)
    })
  
  #######
  output$plot2 <- renderPlot({
    par(mar=c(9,4,1,1))

    TeamFun(team = as.numeric(input$team), data = mydata(), bar=TRUE)
  })
  
  #######
  output$plot3 <- renderPlot({
    TeamFun(team = as.numeric(input$team), data = mydata(), cf=TRUE, bar=FALSE)
  })
  
  #######
  output$plot4 <- renderPlot({
    par(mar=c(9,4,1,1))
    
    TeamFun(team = as.numeric(input$team), data = mydata(), cf=TRUE, bar=TRUE)
  })
  
  #######
  output$plot5 <- renderPlot({
    
    x <- ir[which(as.numeric(ir$team2014) == as.numeric(input$team) ),]
    x <- unique(sort(x$school[which(!is.na(x$immRate[which(x$year == 2013)]))]))
    x <- x[which(x != "NEWBERRY ELEM.")] #NOT SURE WHY NEWBERRY ELEM DOESN'T WORK
    x <- x[1:3]
    
    par(mar=c(5,4,4,1))
    par(mfrow=c(ceiling(length(x)/3), 3))
    
    for (i in x){
      GradeFun(i, 2013)
    }
    
  })
  
  #######
  output$plot6 <- renderPlot({
    
    x <- ir[which(as.numeric(ir$team2014) == as.numeric(input$team) ),]
    x <- unique(sort(x$school[which(!is.na(x$immRate[which(x$year == 2013)]))]))
    x <- x[which(x != "NEWBERRY ELEM.")] #NOT SURE WHY NEWBERRY ELEM DOESN'T WORK
    x <- x[4:6]
    
    par(mar=c(5,4,4,1))
    par(mfrow=c(ceiling(length(x)/3), 3))
    
    for (i in x){
      GradeFun(i, 2013)
    }
    
  })
  
  #######
  output$plot7 <- renderPlot({
    
    x <- ir[which(as.numeric(ir$team2014) == as.numeric(input$team) ),]
    x <- unique(sort(x$school[which(!is.na(x$immRate[which(x$year == 2013)]))]))
    x <- x[which(x != "NEWBERRY ELEM.")] #NOT SURE WHY NEWBERRY ELEM DOESN'T WORK
    x <- x[1:3]
    
    par(mfrow=c(length(x), 2))
    
    for (i in x){
      GradeFun(i, 2013, bar=FALSE)
    }
    
  }, height=1000)
  
  #######
  output$plot8 <- renderPlot({
    
    x <- ir[which(as.numeric(ir$team2014) == as.numeric(input$team) ),]
    x <- unique(sort(x$school[which(!is.na(x$immRate[which(x$year == 2013)]))]))
    x <- x[which(x != "NEWBERRY ELEM.")] #NOT SURE WHY NEWBERRY ELEM DOESN'T WORK
    x <- x[4:6]
    
    par(mfrow=c(length(x), 2))
    
    for (i in x){
      GradeFun(i, 2013, bar=FALSE)
    }
    
  }, height = 1000)
  
  
  
  
  

  ########
  output$text1 <- renderText({
    paste("Team", input$team, "immunization rate overview")
  })
  
  ########
  output$text2 <- renderText({
    paste("Team", input$team, "consent form return rate overview")
  })
  

  ########
  output$table1 <- renderDataTable({
    x <- as.data.frame(mydata())
    x[which(x$team2014 == input$team),]
    x <- x[,c("school", "year", "immRate", "cfrr", "totMem")]
    x <- x[order(x$school),]
  })
  
  ########
  output$motionchart1 <- renderGvis({
    
    gvisMotionChart(data = mydata(), 
                    idvar = "school", 
                    timevar = "year",
                    xvar = "year",
                    yvar = "immRate",
                    colorvar = "team2014",
                    sizevar = "totMem")

  })
  
  

})