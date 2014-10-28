shinyUI(fluidPage(
  titlePanel("ATE plot generator"),
  tabsetPanel(id="tabs",          
              #@@@@@@@@@@@@@@@@@@
              tabPanel("Data upload",
                       #mainPanel(
                         paste("Upload your data here.  Once it looks good, go to one of the plot tabs."),
                         tableOutput('contents')#,
                         #textOutput("texta")
                       #)
              ),
              #@@@@@@@@@@@@@@@@@@
              tabPanel("Plot (with subgroups)",       
                       mainPanel(
           
                         #paste(colnames(df))
                      plotOutput("plot1")

                         )
              ),
              
              #@@@@@@@@@@@@@@@@@@
              tabPanel("Plot (no subgroups)",       
                       mainPanel(
                         
                         #paste(colnames(df))
                         plotOutput("plot2")
                         
                       )
              ),
              
              sidebarPanel(
                conditionalPanel(
                  condition = "input.tabs == 'Data upload'",
                  fileInput('file1', 'Choose file to upload',
                            accept = c(
                              'text/csv',
                              'text/comma-separated-values',
                              'text/tab-separated-values',
                              'text/plain',
                              '.csv',
                              '.tsv'
                            )
                  ),
                  tags$hr(),
                  checkboxInput('header', 'Header', TRUE),
                  radioButtons('sep', 'Separator',
                               c(Comma=',',
                                 Semicolon=';',
                                 Tab='\t'),
                               ','),
                  radioButtons('quote', 'Quote',
                               c(None='',
                                 'Double Quote'='"',
                                 'Single Quote'="'"),
                               '"'),
                  tags$hr()
                ),
                conditionalPanel(
                  condition = "input.tabs == 'Plot (with subgroups)'",
                  
                  textInput("modform", "Model form", value = "response ~ predictor.1 + predictor.2 + treat"),
                  
                  textInput("treatment", "Treatment variable", value = "treat"),
                  
                  textInput("subgroups", "Subgroups", value = "sex"),
                  
                  
                  
                  selectInput("method", "Method",
                              c("logit" = "logit",
                                "probit" = "probit",
                                "multinomial (not yet implemented)" = "mlogit")),
                  
                  
                  selectInput("color", "Bar color:",
                              c("Green" = "darkgreen",
                                "Red" = "darkred",
                                "Black" = "black",
                                "Blue" = "darkblue")),
                  
                  sliderInput("level", "Confidence level",
                              min=80, max=99, value=95, step=1),
                  
                  sliderInput("alpha.bar", "Bar opaqueness",
                              min=0.1, max=1, value=0.4, step=0.1),
                  
                  sliderInput("alpha.text", "Text opaqueness",
                              min=0.1, max=1, value=0.6, step=0.1),
                  
                  sliderInput("alpha.ci", "Error bar opaqueness",
                              min=0.1, max=1, value=0.3, step=0.1),
                  
                  selectInput("errbar.col", "Error bar color:",
                              c("Red" = "darkred",
                                "Black" = "black",
                                "Green" = "darkgreen",
                                "Blue" = "darkblue")),
                  
                  selectInput("label.ci", "Print confidence interval values?",
                              c("No" = FALSE,
                                "Yes" = TRUE))
                  
                  ),
                
                conditionalPanel(
                  condition = "input.tabs == 'Plot (no subgroups)'",
                  
                  textInput("modform2", "Model form", value = "response ~ predictor.1 + predictor.2 + treat"),
                  
                  textInput("treatment2", "Treatment variable", value = "treat"),          
                  
                  
                  selectInput("method2", "Method",
                              c("logit" = "logit",
                                "probit" = "probit",
                                "multinomial (not yet implemented)" = "mlogit")),
                  
                  
                  selectInput("color2", "Bar color:",
                              c("Black" = "black",
                                "Red" = "darkred",
                                "Green" = "darkgreen",
                                "Blue" = "darkblue")),
                  
                  sliderInput("level2", "Confidence level",
                              min=80, max=99, value=95, step=1),
                  
                  sliderInput("alpha.bar2", "Bar opaqueness",
                              min=0.1, max=1, value=0.3, step=0.1),
                  
                  sliderInput("alpha.text2", "Text opaqueness",
                              min=0.1, max=1, value=0.6, step=0.1),
                  
                  sliderInput("alpha.ci2", "Error bar opaqueness",
                              min=0.1, max=1, value=0.3, step=0.1),
                  
                  selectInput("label.ci2", "Print confidence interval values?",
                              c("No" = FALSE,
                                "Yes" = TRUE))
                  
                )
                
                )
  )  
)
)

                       
 