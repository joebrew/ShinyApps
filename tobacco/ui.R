library(shiny)
library(rCharts)
suppressPackageStartupMessages(library(googleVis))

shinyUI(fluidPage(
  
  # Application title
  headerPanel("Tobacco data explorer"),
  
  sidebarPanel( 
#     tags$a(href="mailto:joseph.brew@flhealth.gov", "Joe Brew"                        
#     
#     #tags$img(src="https://docs.google.com/drawings/d/1z5OtlqZ8ex1QhHDtsCOvYMM2WgFsYgD1onjSi-Kus40/pub?w=534&h=89", height="100px"
#     ),

    
    selectInput("xvar", "First variable", c(
                #paste0("'",as.character(t(comb_names[1,])), "'='", names(comb), "")
                
                #"County" = 1,
                #"Interview date" = 2,
                #"Employer" = 3,
                #"Number of employees" = 4,
                "Business size" = 5,
                #"Number of tobacco users" = 6,
                "Does employer hire tobacco users?" = 7,
                "Does employer provide health insurance?" = 8,
                "Type of insurance" = 9,
                "Insurance carrier" = 10,
                "Employer sector" = 11,
                "Any tobacco cessation coverage?" = 12,
                "Patch NRT OTC covered?" = 13,
                "Gum NRT OTC covered?" = 14,
                "Lozenge NRT OTC covered?" = 15,
                "OTC covered only?" = 16,
                "Inhaler NRT Rx covered?" = 17,
                "Nose spray Rx covered?" = 18,
                "Bupropion Rx covered?" = 19,
                "varenicline Rx covered?" = 20,
                "Covered Rx only?" = 21,
                "Covered both OTC and Rx?" = 22,
                "Individual counseling covered?" = 23,
                "Phone counseling covered?" = 24,
                "Group counseling covered?" = 25,
                "Web based counseling covered?" = 26,
                "Covered some types of counseling?" = 27,
                "Covered all types of counseling?" = 28,
                "Covered at least 2 quit attempts per year?" = 29,
                "Combination counseling and meds covered?" = 30,
                "Any barriers to coverage of meds?" = 31,
                "Extent of TFG policy coverage" = 32,
                "E-cigs restricted by policy" = 33,
                "Sliver FTCA" = 34,
                "Gold FTCA" = 35#,
#                 "Baseline SOC benefit" = 36,
#                 "Baseline SOC TFG policy" = 37,
#                 "Baseline SOC promote and encourage" = 38,
#                 "Baseline referral made to AHEC" = 39,
#                 "Q1 SOC benefit" = 40,
#                 "Q1 SOC TFG policy" = 41,
#                 "Q1 SOC promote and encourage" = 42,
#                 "Q1 referral to local AHEC" = 43,
#                 "Q2 SOC benefit" = 44,
#                 "Q2 TFG policy" = 45,
#                 "Q2 promote and encourage" = 46,
#                 "Q2 referral made to AHEC" = 47,
#                 "Q3 SOC benefit" = 48,
#                 "Q3 TFG policy" = 49,
#                 "Q3 promote and encourage" = 50,
#                 "Q3 referral to local AHEC" = 51,
#                 "Q4 SOC benefit" = 52,
#                 "Q4 SOC TFG policy" = 53,
#                 "Q4 promte and encourage" = 54,
#                 "Q4 referral to local AHEC" = 55,
#                 "Describe progress" = 56,
#                 "Plans to enage" = 57
                )),
    
    selectInput("yvar", "Cross variable (optional)", c(
      #paste0("'",as.character(t(comb_names[1,])), "'='", names(comb), "")
      "None" = 0,
      #"County" = 1,
      #"Interview date" = 2,
      #"Employer" = 3,
      #"Number of employees" = 4,
      "Business size" = 5,
      #"Number of tobacco users" = 6,
      "Does employer hire tobacco users?" = 7,
      "Does employer provide health insurance?" = 8,
      "Type of insurance" = 9,
      "Insurance carrier" = 10,
      "Employer sector" = 11,
      "Any tobacco cessation coverage?" = 12,
      "Patch NRT OTC covered?" = 13,
      "Gum NRT OTC covered?" = 14,
      "Lozenge NRT OTC covered?" = 15,
      "OTC covered only?" = 16,
      "Inhaler NRT Rx covered?" = 17,
      "Nose spray Rx covered?" = 18,
      "Bupropion Rx covered?" = 19,
      "varenicline Rx covered?" = 20,
      "Covered Rx only?" = 21,
      "Covered both OTC and Rx?" = 22,
      "Individual counseling covered?" = 23,
      "Phone counseling covered?" = 24,
      "Group counseling covered?" = 25,
      "Web based counseling covered?" = 26,
      "Covered some types of counseling?" = 27,
      "Covered all types of counseling?" = 28,
      "Covered at least 2 quit attempts per year?" = 29,
      "Combination counseling and meds covered?" = 30,
      "Any barriers to coverage of meds?" = 31,
      "Extent of TFG policy coverage" = 32,
      "E-cigs restricted by policy" = 33,
      "Sliver FTCA" = 34,
      "Gold FTCA" = 35#,
#       "Baseline SOC benefit" = 36,
#       "Baseline SOC TFG policy" = 37,
#       "Baseline SOC promote and encourage" = 38,
#       "Baseline referral made to AHEC" = 39,
#       "Q1 SOC benefit" = 40,
#       "Q1 SOC TFG policy" = 41,
#       "Q1 SOC promote and encourage" = 42,
#       "Q1 referral to local AHEC" = 43,
#       "Q2 SOC benefit" = 44,
#       "Q2 TFG policy" = 45,
#       "Q2 promote and encourage" = 46,
#       "Q2 referral made to AHEC" = 47,
#       "Q3 SOC benefit" = 48,
#       "Q3 TFG policy" = 49,
#       "Q3 promote and encourage" = 50,
#       "Q3 referral to local AHEC" = 51,
#       "Q4 SOC benefit" = 52,
#       "Q4 SOC TFG policy" = 53,
#       "Q4 promte and encourage" = 54,
#       "Q4 referral to local AHEC" = 55,
#       "Describe progress" = 56,
#       "Plans to enage" = 57
    )),
    
    sliderInput("cex.names", "Size of names", 
                min=0.1, max=2, value=1, step=0.1),
    
    sliderInput("legend.cex", "Size of legend", 
                min=0.1, max=2, value=1, step=0.1),
    
    sliderInput("err.cex", "Size of confidence text", 
                min=0.1, max=2, value=0.7, step=0.1),
    
    radioButtons("las","Orientation of names",c(
      "Horizontal"="1",
      "Vertical"="3"),
      selected = 3),
    
    radioButtons("legend","Legend",c(
      "Yeah!"=TRUE,
      "Nope"=FALSE)),
    
    radioButtons("rain","Rainbow colors",c(
      "Yeah!"=TRUE,
      "Nope"=FALSE),
      selected = FALSE),
    
#     radioButtons("percent","Percent or absolute",c(
#       "percent"=TRUE,
#       "absolute"=FALSE)),
    
    textInput("legend.title", "Legend title",
              value = NULL),
    
    textInput("ylab", "Y axis label",
              value = "Percent"),

    selectInput("county", "County for dynamic map (select before selecting tab)",
                c("State",
                  "Alachua",
                  "Baker",
                  "Bay",
                  "Bradford",
                  "Brevard",
                  "Broward",
                  "Calhoun",
                  "Charlotte",
                  "Citrus",
                  "Clay",
                  "Collier",
                  "Columbia",
                  "DeSoto",
                  "Dixie",
                  "Duval",
                  "Escambia",
                  "Flagler",
                  "Franklin",
                  "Gadsden",
                  "Gilchrist",
                  "Glades",
                  "Gulf",
                  "Hamilton",
                  "Hardee",
                  "Hendry",
                  "Hernando",
                  "Highlands",
                  "Hillsborough",
                  "Holmes",
                  "Indian River",
                  "Jackson",
                  "Jefferson",
                  "Lafayette",
                  "Lake",
                  "Lee",
                  "Leon",
                  "Levy",
                  "Liberty",
                  "Madison",
                  "Manatee",
                  "Marion",
                  "Martin",
                  "Miami-Dade",
                  "Monroe",
                  "Nassau",
                  "Okaloosa",
                  "Okeechobee",
                  "Orange",
                  "Osceola",
                  "Palm Beach",
                  "Pasco",
                  "Pinellas",
                  "Polk",
                  "Putnam",
                  'Santa Rosa',
                  "Sarasota",
                  "Seminole",
                  "St. Johns",
                  "St. Lucie",
                  "Sumter",
                  "Suwannee",
                  "Taylor",
                  "Union",
                  "Volusia",
                  "Wakulla",
                  "Walton",
                  "Washington"))
    
    
    
    
    #     htmlOutput("selectUI")
    
    #     selectInput("grade", "Grade",c(
    #       "Pre-k"="-1",
    #       "K"="0",
    #       "1"="1",
    #       "2"="2",
    #       "3"="3",
    #       "4"="4",
    #       "5"="5",
    #       "5"="5",
    #       "6"="6",
    #       "7"="7",
    #       "8"="8",
    #       "9"="9",
    #       "10"="10",
    #       "11"="11",
    #       "12"="12"      
    #       ))
    
    #     sliderInput("year", "Year", 
    #                 min=2006, max=2013, value=2006, step=1,
    #                 animate=animationOptions(interval = 3000, loop = FALSE,
    #                                          playButton = "Play", pauseButton = "Pause"))
    
    #submitButton(text="Ready")
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Barcharts",
               #htmlOutput("motionchart1")
               plotOutput("plot1")
      ),
      

      #tabPanel("Details",htmlOutput("linech"),
      
      tabPanel("Static maps",
               #h4(textOutput("text2"), align="center"),
               plotOutput("plot2"),
               plotOutput("plot3")),
      
      tabPanel("Interactive map", tags$style('.leaflet {height: 400px;}'),showOutput('myChart2', 'leaflet'))
      

      
      #,
      #tabPanel("Details",htmlOutput("linech")
    )
  )
))