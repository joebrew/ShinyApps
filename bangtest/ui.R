shinyUI(
  pageWithSidebar(
    
    headerPanel("Test"),
    
    sidebarPanel(
      br(),
      br(),
      downloadButton("daily",label=h4("Build report")),
      br()
    ),
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Console",
                  verbatimTextOutput('console')
                  )
      )
    )
  
  )
)