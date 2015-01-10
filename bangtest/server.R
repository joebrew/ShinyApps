library(shiny)
library(knitr)

data <- c(1,2,3,3,4,4,5)

shinyServer(function(input,output){

  output$console <- renderText({"Hopefully this works."})

output$daily <-
  downloadHandler(filename = "report.pdf",
                  content = function(file){
                    # generate PDF
                    Sweave2knitr("bangtest.Rnw",output="report.Rnw")
                    knit2pdf("report.Rnw")
                    
                    # copy pdf to 'file'
                    file.copy("report.pdf", file)
                    
                    # delete generated files
                    file.remove("report.pdf", "report.tex",
                                "report.aux", "report.log")
                    
                    # delete folder with plots
                    #unlink("figure", recursive = TRUE)
                  },
                  contentType = "application/pdf"
  )

})