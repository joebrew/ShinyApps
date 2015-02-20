#########################
# SET WD TO LOCAL
#########################
#setwd("C:/Users/BrewJR/Documents/")
#setwd("./analystInstitute/Rtools")
#setwd("/home/joebrew/ShinyApps/ate")

#########################
# LOAD NECESSARY PACKAGES
#########################
require(nnet)
#require(scales)
library(Hmisc) #note, not using ggplot

#########################
# SOURCE SCRIPT
#########################
source("helper.R", echo=FALSE)


##############


# Define server logic for slider examples
shinyServer(function(input, output) {
  
  
  #DATA READ IN
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    df <-
      read.csv(inFile$datapath, header = input$header,
             sep = input$sep, quote = input$quote)
    
    head(df)
  })
  
  #######################
  # PLOT
  output$plot1 <- renderPlot(
#    height=600,
#    width=900,
    {
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    df <-
      read.csv(inFile$datapath, header = input$header,
               sep = input$sep, quote = input$quote)
    
    #hist(df$predictor.2)
    #barplot(summary(factor(df$predictor.2)))
    
    ate.plot.joe(modform =input$modform,  #response ~ predictor.1 + predictor.2 + treat, 
                 data=df, 
                 level=input$level/100, 
                 method=input$method, 
                 #treatment="treat",
                 treatment=input$treatment,
                 #response="response",
                 response= gsub(" ", "", unlist(strsplit(input$modform, "~"))[1]),
                 color=input$color,
                 errbar.col=input$errbar.col,
                 alpha.text=input$alpha.text,
                 alpha.ci=input$alpha.ci,
                 alpha.bar=input$alpha.bar,
                 label.ci=input$label.ci,
                 signif.stars=TRUE,
                 print.data=FALSE,
                 #subgroups=NULL, 
                 #subgroups= "predictor.4",
                 subgroups=input$subgroups,
                 dims=NULL, 
                 file=NULL, 
                 title=NULL)

    })
  
  
  #######################
  # PLOT
  output$plot2 <- renderPlot({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    df <-
      read.csv(inFile$datapath, header = input$header,
               sep = input$sep, quote = input$quote)
    
    #hist(df$predictor.2)
    #barplot(summary(factor(df$predictor.2)))
    
    ate.plot.joe(modform =input$modform2,  #response ~ predictor.1 + predictor.2 + treat, 
                 data=df, 
                 level=input$level2/100, 
                 method=input$method2, 
                 #treatment="treat",
                 treatment=input$treatment2,
                 #response="response",
                 response= gsub(" ", "", unlist(strsplit(input$modform2, "~"))[1]),
                 color=input$color2,
                 errbar.col=input$errbar.col,
                 alpha.text=input$alpha.text2,
                 alpha.ci=input$alpha.ci2,
                 alpha.bar=input$alpha.bar2,
                 label.ci=input$label.ci2,
                 signif.stars=TRUE,
                 print.data=FALSE,
                 subgroups=NULL, 
                 #subgroups= "predictor.4",
                 #subgroups=input$subgroups,
                 dims=NULL, 
                 file=NULL, 
                 title=NULL)
    
  })


  
  #############################
  output$plotb <- renderPlot({
    
    plot(1:10, 1:10)
    

  } )

#############################
output$texta <- renderText({
  
  inFile <- input$file1
  
  if (is.null(inFile))
    return(NULL)
  
  df <-
    read.csv(inFile$datapath, header = input$header,
             sep = input$sep, quote = input$quote)
  

  print(mean(df[,3]))
  
} )
  
  

  
  
  ################################
  output$tablea = renderTable({
    
    outdf <- read.csv(input$df$name[1]) 
    
    outdf[1:5,]
  })



  
  
})