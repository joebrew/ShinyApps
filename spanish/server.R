
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(RCurl)
my_link <- "https://docs.google.com/spreadsheets/d/1voN1R9jqfVnIhlhOZdMpCrWHeFssk6J02uMkf8IT1oo/export?gid=0&format=csv"
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
my_csv <- getURL(my_link)
dat <- read.csv(textConnection(my_csv), stringsAsFactors = FALSE)

subjects <- dat$subject[which(nchar(dat$subject) > 0)]
verbs <- dat$verb[which(nchar(dat$verb) > 0)]
forms <- dat$form[which(nchar(dat$form) > 0)]

library(shiny)

shinyServer(function(input, output) {

  output$table1 <- renderTable({

    # SUBJECT
   subject <- sample(subjects, 10, replace = TRUE)
   
   
   # VERB
   if(input$verb_type != "both"){
     verb <- sample(verbs[which(dat$type == input$verb_type)], 10, replace = TRUE)
   } else {
     verb <- sample(verbs, 10, replace = TRUE)
   }
   
   # FORM
     form <- sample(forms, 10, replace = TRUE)
  
    
   data.frame(subject, verb, form)
  })

})
