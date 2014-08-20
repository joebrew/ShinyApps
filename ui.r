# Define UI for slider demo application
shinyUI(pageWithSidebar(
  
  ####################################
  headerPanel("Lead poisoning in Chicago"),

 ############################



  ####################################
  mainPanel(
    
    tabsetPanel(id="tabs",
      
      
      #@@@@@@@@@@@@@@@@@@
      tabPanel("Overview",
     
               
               plotOutput(
                 "plota"
               ),
               
               
               plotOutput(
                 "plotb"
               ),
               
               plotOutput(
                 "plotc"
               )
      ),
      
      #@@@@@@@@@@@@@@@@@@
      tabPanel("By year",
               h2(
                 textOutput("text1"),
                 align="center"
               ),
               

               plotOutput(
                 "plot1"
               ),
               
               
               plotOutput(
                 "plot2"
                 )
      ),
          

    
    #@@@@@@@@@@@@@@@@@@
    tabPanel("Bias",
             plotOutput(
               "plot5"
             )
    ),
    
    #@@@@@@@@@@@@@@@@@@
    tabPanel("Models",
             plotOutput("splot1"),
             tableOutput("table1"),
             strong(textOutput("txt1")),
             strong(textOutput("txt2")),
             strong(textOutput("txt3")),
             plotOutput("splot2")
             
    )
    
    )     
  ), #
 
 
 ####################################
 sidebarPanel(
   
   conditionalPanel(
     condition = "input.tabs == 'By year'",
     
     # YEAR
     sliderInput("year", "Year", 
                 min=1995, max=2013, value=1995, step=1,
                 animate=animationOptions(interval = 3000, loop = FALSE,
                                          playButton = "Play", pauseButton = "Pause")),
     

     #     
     #YVARYEAR
     selectInput("yvaryear", "Variable",
                 c("Mean BLL" = "mean.bll",
                   "Number of tests > 10 ug/dL"= "nTestsOver10",
                   "Number of tests > 20 ug/dL"= "nTestsOver20",
                   "Percent > 10 ug/dL" = "pTestsOver10",
                   "Percent > 20 ug/dL" = "pTestsOver20",
                   "Median BLL" = "median.bll",
                   "Total number of tests" = "nTests"))
     
   ),
   
   #helpText("Pick some parameters"),
   
   conditionalPanel(
     condition = "input.tabs == 'Overview'",
     
     #XVAR
     selectInput("xvar", "X variable",
                 c("Mean BLL" = "mean.bll",
                   "Interior hazard rate" = "interiorTF", 
                   "Exterior hazard rate" = "exteriorTF", 
                   "Number of tests > 10 ug/dL"= "nTestsOver10",
                   "Number of tests > 20 ug/dL"= "nTestsOver20",
                   "Percent > 10 ug/dL" = "pTestsOver10",
                   "Percent > 20 ug/dL" = "pTestsOver20",
                   "Median BLL" = "median.bll",
                   "Total number of tests" = "nTests",
                   "Percent poverty" = "PtPov",
                   "Percent college graduation" = "PtBAPlus",
                   "Percent white" = "PtNLWh",
                   "Percent black" = "PtNLB", 
                   "Percent latino" = "PtL",
                   "Mean age at first test" = "meanAgeFirstTest",
                   "Median age at first test" = "medianAgeFirstTest"),
                 selected="PtPov"),
     
     #     
     #VAR
     selectInput("yvar", "Y variable",
                 c("Mean BLL" = "mean.bll",
                   "Interior hazard rate" = "interiorTF", 
                   "Exterior hazard rate" = "exteriorTF", 
                   "Number of tests > 10 ug/dL"= "nTestsOver10",
                   "Number of tests > 20 ug/dL"= "nTestsOver20",
                   "Percent > 10 ug/dL" = "pTestsOver10",
                   "Percent > 20 ug/dL" = "pTestsOver20",
                   "Median BLL" = "median.bll",
                   "Total number of tests" = "nTests",
                   "Percent poverty" = "PtPov",
                   "Percent college graduation" = "PtBAPlus",
                   "Percent white" = "PtNLWh",
                   "Percent black" = "PtNLB", 
                   "Percent latino" = "PtL",
                   "Mean age at first test" = "meanAgeFirstTest",
                   "Median age at first test" = "medianAgeFirstTest"),
                 selected="ptNLWh"),
                 


     
     
     #BUBBLE SHADING
     radioButtons("bub", "Bubble shading",
                 list("Percent poverty" = "PtPov",
                   "Percent white" = "PtNLWh",
                   "Percent black" = "PtNLB", 
                   "Percent latino" = "PtL",
                   "Percent college graduation" = "PtBAPlus",
                   "Interior hazard rate" = "interiorTF", 
                   "Exterior hazard rate" = "exteriorTF"),
                 selected="PtNLB"),
     
     #BREAKS
     sliderInput("breaks", "Map breaks", 
                 min=3, max=9, value=9, step=1)
     
   ),
   
   
   
   
   
   
   conditionalPanel(
     condition = "input.tabs == 'Bias'",
     

     #COLOR
     selectInput("col", "Do you like colors?",
                 c("Yes" = "TRUE",
                    "No" = "FALSE"),
                 selected="FALSE")
     

     
   ),
   #############################
   
   
   
   conditionalPanel(
     condition = "input.tabs == 'Models'",
   
     selectInput("resp",
                 label = strong("What to predict"),
                 choices = c("Interior hazard"=1,
                             "Exterior hazard"=2,
                             "Compliance status"=3,
                             "Blood lead level>5"=4,
                             "Blood lead level>10"=5,
                             "Blood lead level>20"=6),
                 selected = 1),
     
     selectInput("model",
                 label = strong("Method for predictive model"),
                 choices = c("Logistic Regression"=1,
                             "Classification Tree"=2,
                             "Bagging 10"=3,
                             "Bagging 25"=4,
                             "Naive Bayes"=5),
                 selected = 1)
     )
   
   

   
   
   
 ) #######,
 
 
 
 
))
