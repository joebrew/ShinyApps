shinyUI(fluidPage(
  tags$div(title= "This power calculator is a planning tool for designing experiments. If you enter the aspects of your experimental design, the calculator will tell you the effect sizes that your treatment (e.g., voter outreach) needs to generate for you to likely find statistically significant effects when analyzing the results of your experiment.",
           titlePanel("Power calculator")),
  
  
  tabsetPanel(id="tabs",          
              #@@@@@@@@@@@@@@@@@@
              tabPanel("Dichotomous outcome",
                       mainPanel(
                         
                         tags$div(title="If you enter in your universe sizes and other features of your program, the calculator will tell you (in the graph) the \"minimal detectable effect\" (MDE) that you can expect to observe with your design. For instance, if you specify a power of 80% and a confidence interval of 90% (along with other aspects of your design), and the calculator shows you an MDE of 2 percentage points, that means if your treatment effect is truly 2 percentage points then 80% of the time you run your experiment, you will identify a statistically significant effect at the 90% confidence level.",
                                  h4("Minimum detectable effects", align="center")),
                         
                         plotOutput("dplot"),
                         br(),
                         
                         tags$div(title ="The Intent to Treat (ITT) effect is the treatment effect (e.g., impact of voter contact) on everyone assigned to the treatment condition. Think of this effect as the impact of a canvass on a precinct; even though you won't talk to everyone in the precinct, the canvass will have a small \"ITT\" effect on the precinct as a whole.",
                                  p("ITT: intention to treat", align="center")),
                         
                         tags$div(title ="The Treatment on Treated (ToT) effect is the treatment effect (e.g., impact of voter contact) on individuals who actually received the intended treatment. For instance, think of this effect as the impact of a canvass on an inviduals who answered their door and had a conversation with the canvasser. For intensive treatments, such as a personal canvass, the ToT effect might be quite large. The ToT effect is the ITT effect multiplied by the contact rate of your treatment.",
                                  p("ToT: treatment on treated", align="center")),
                         
                         tags$div(title ="Percentage points",
                                  p("pp: percentage points", align="center")),
                         
                         
                         br(),
                         br(),
                         
                         h4(textOutput("Details")),
                         h5(textOutput("Number.of.outcome.measurements")),
                         h4(textOutput("b5")),
                         h5(textOutput("S.E.")),
                         h4(textOutput("b14")),
                         h5(textOutput("ITT.MDE")),
                         h4(textOutput("b16")),
                         h5(textOutput("TOT.MDE")),
                         h4(textOutput("b17"))
                         #br(),                   
                         #                          h5(textOutput("Average.cluster.size")),
                         #                          h4(textOutput("b20")),
                         #                          h5(textOutput("Square.root.of.variance.inflation.factor")),
                         #                          h4(textOutput("b22")),
                         #                          h5(textOutput("Adjusted.S.E.")),
                         #                          h4(textOutput("b23")),
                         #                          h5(textOutput("Adj.ITT.MDE")),
                         #                          h4(textOutput("b25")),
                         #                          h5(textOutput("Adj.TOT.MDE")),
                         #                          h4(textOutput("b26"))
                         
                         
                       )
              ),
              #@@@@@@@@@@@@@@@@@@
              tabPanel("Continuous outcome" ,       
                       mainPanel(
                         
                         p("We would like to better understand how people would use a continuous outcome power calculator because different scenarios would require different statistical assumptions. If you are interested, please contact Josh Berezin:"),
                         tags$a(href="mailto:jberezin@analystinstitute.org", "jberezin@analystinstitute.org")                         
                         
                         
                       )
              ),
              
              
              
              sidebarPanel(
                
                ###############################
                conditionalPanel(
                  condition = "input.tabs == 'Dichotomous outcome'",
                  
                  tags$div(title="The universe size is the toal number of individuals involved in the experiment, regardless of whether they are in the treatment or control group. The treatment group consists of everyone you will attempt to contact, not just those who are successfully reached.",
                           textInput("total.n", "Universe size (total n)", value = 20000)),
                  
                  
                  
                  
                  tags$div(title="The percent of individuals in your universe who you will collect an outcome variable for. Example 1: for a GOTV study on registered voters with the voter rolls being the outcome, the outcome collection rate will be 100% because you will know whether every registered voter did or did not vote. Example 2: for a persuasion experiment with a survey as the outcome measure, indicate your expected survey response rate here (perhaps in the 5%-15% range -- don't forget that some of your phone numbers may be disconnected or wrong)." ,
                           sliderInput("dv.variable.contact.rate", "Outcome collection rate (%) (e.g., survey response rate)",
                                       min=0, max=100, value=100, step=1)),
                  
                  
                  br(),
                  
                  tags$div(title="The percent of individuals in your universe who are assigned to the treatment group.",                           
                           sliderInput("percent.in.treatment", "Percent (%) in treatment group",
                                       min=0, max=100, value=50, step=1)),
                  
                  
                  tags$div(title="The percent of individuals in the control group expected to act in the desired fashion. For GOTV experiments, this is the percent of the control group who votes. For registration experiments, it's the percent who register on their own. For persuasion experiments, it might be the percent who support your candidate. For email A/B experiments, it's the percent of recipients in the B group who take the desired action (e.g., open rate, donation rate).",
                           sliderInput("baseline.action.support.rate", "Turnout/support/action rate in control (%)",
                                       min=0, max=100, value=50, step=1)),
                  
                  tags$div(title="The contact (or \"treatment application\" rate) is the percent of individuals assigned to be treated who are actually treated. For a mail experiment, this might be as high as 95% (if 5% of addresses are bad). For canvass experiments, your canvass contact rate might be between 10% and 20% -- note this is the percent of canvass conversations out of all targets; it is *not* the ratio of conversations to doors knocked.",
                           sliderInput("treatment.application.rate", "Contact (i.e, treatment application) rate (%)",
                                       min=0, max=100, value=100, step=1)),
                  
                  
                  tags$div(title="Inexperienced users: keep at 0. Experienced users: if you have covariates that can explain some of the natural variance of actions (e.g., propensity scores for GOTV experiments, partisanship scores for persuasion experiments), then estimate the cumulative r-squared of those covariates here. For panel experiments, where you pre-survey respondents, estimating an r-sqaured of 0.9 for the pre-survey on the post-survey is reasonable.",
                           sliderInput("r.squared", "Predictive power of individual-level covariates (R-squared)",
                                       min=0, max=1, value=0.0, step=0.01)),
                  
                  
                  tags$div(title="For advanced users: the percent of the time that you're aiming to find statsitically significant results (at the confidence level set below). Do not set below 50 or at 100.",
                           sliderInput("power", "Desired power of experiment",
                                       min=0, max=100, value=80, step=1)),
                  
                  
                  tags$div(title="For advanced users: the two-tailed confidence interval with which \"statistical signifance\" will be calculated. We do not recommend values below 75 or above 95.",
                           sliderInput("confidence.interval", "Confidence interval to use (2-tailed)",
                                       min=0, max=100, value=90, step=1)),
                  
                
                  
                  checkboxInput("checkbox", label = "Show details", value = FALSE),
                  
                  "",
                  
                  tags$img(src="https://analystinstitute.org/wp-content/themes/analystg/images/analyst-logo.png", height="500px")
                  
                  
                ),
                
                ###########################
                conditionalPanel(
                  condition = "input.tabs == 'Continuous outcome'",
                  
                  
                
                  tags$img(src="https://analystinstitute.org/wp-content/themes/analystg/images/analyst-logo.png", height="500px")
                  
                  
                ) #END OF CONT OUTCOME CONDITIONAL PANEL
                
                
              )
  )
)
)
