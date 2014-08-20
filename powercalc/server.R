library(shiny)

#########################
# SOURCE IN HELPER FUNCTION
##############



# Define server logic for slider examples
shinyServer(function(input, output) {
  
  source("PowerFun.R")
  
  mydata <- reactive({
    PowerFun(total.n = as.numeric(input$total.n), 
             dv.variable.contact.rate = input$dv.variable.contact.rate,
             percent.in.treatment = input$percent.in.treatment,
             baseline.action.support.rate = input$baseline.action.support.rate,
             treatment.application.rate = input$treatment.application.rate,
             r.squared = input$r.squared,
             power = input$power,
             confidence.interval = input$confidence.interval,
             #number.of.clusters = input$number.of.clusters,
             #intra.cluster.correlation.coefficient = input$intra.cluster.correlation.coefficient,
             checkbox = input$checkbox)     
  })
  
  
  
  
  
  
  # HIDDEN DETAILS
  
  output$Details <- renderText({ 
    if(input$checkbox){
      print("Details")
    }
  })
  
  
  output$Number.of.outcome.measurements <- renderText({ 
    if(input$checkbox){
      print("Number of outcome measurements:")
    }
  })
  
  output$b5 <- renderText({ 
    if(input$checkbox){
      print(mydata()$myValues[which(mydata()$myNames == "b5")])
    }
  })
  
  
  
  #############################
  output$S.E. <- renderText({ 
    if(input$checkbox){
      print("S.E.:")
    }
  })
  output$b14 <- renderText({ 
    if(input$checkbox){
      
      print(mydata()$myValues[which(mydata()$myNames == "b14")])
    }
  })
  
  #############################
  output$ITT.MDE <- renderText({ 
    if(input$checkbox){
      print("ITT MDE:")
    }
  })
  output$b16 <- renderText({ 
    if(input$checkbox){
      
      print(mydata()$myValues[which(mydata()$myNames == "b16")])
    }
  })
  
  
  #############################
  output$TOT.MDE <- renderText({ 
    if(input$checkbox){
      print("ToT MDE:")
    }
  })
  output$b17 <- renderText({ 
    if(input$checkbox){
      print(mydata()$myValues[which(mydata()$myNames == "b17")])
    }
  })
  
  ################### END OF DETAILS SECTION
  
  #b20
  output$b20 <- renderText({
    print(mydata()$myValues[which(mydata()$myNames == "b20")])
  })
  
  #b22
  output$b22 <- renderText({
    print(mydata()$myValues[which(mydata()$myNames == "b22")])
    
  })
  
  #b23
  output$b23 <- renderText({
    print(mydata()$myValues[which(mydata()$myNames == "b23")])
    
  })
  
  #b25
  output$b25 <- renderText({
    print(mydata()$myValues[which(mydata()$myNames == "b25")])
    
  })
  
  #b26
  output$b26 <- renderText({
    print(mydata()$myValues[which(mydata()$myNames == "b26")])
    
  })
  
  
  
  
  ############### BARPLOT
  output$dplot <- renderPlot({ 
    
    myCols <- adjustcolor(c( "darkgreen", "grey"), alpha.f=0.5)
    
    bp.elements <- c(mydata()$myValues[which(mydata()$myNames == "b16")],
                     mydata()$myValues[which(mydata()$myNames == "b17")])
    
    #         bp.elements <- c(mydata()["b16",],
    #                          mydata()["b17",])
    bp.names <- c("ITT",
                  "ToT")
    
    mybp <- barplot(bp.elements*100,
                    col=myCols,
                    names.arg=bp.names,
                    ylab="Minimum detectable effects (pp)",
                    main=NULL,
                    border=FALSE)
    
    #legend(x="topright", fill=c("yellow", "green"), legend=bp.elements)
    text(x=mybp[,1], y=bp.elements*100,
         pos= ifelse(bp.elements >= 0.95*max(bp.elements, na.rm=TRUE),1,3),
         labels=paste(round(bp.elements*100, digits=2), "pp"),
         cex=1.5)
    box("outer")
    
  })
  
  
})