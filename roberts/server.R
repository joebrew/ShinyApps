library(shiny)
library(dplyr)
suppressPackageStartupMessages(library(googleVis))


shinyServer(function (input, output#, 
                      #session
                      ) {
  
  # No scientific
  options(scipen=999)
  
  # Source data on imm rates
  source("helper.R")
  
  # Source absenteeism
  load('school_absenteeism.RData')
  
  # Merge together
  school <- left_join(x = ir, y = school, by = c("id", "year"))

  # Clean up
  good_columns <- c("id", "school.x", "doses", "totMem", "immRate", "year", "type.x",
                    "pubPriv.x", "frLunch13", "percent_free_reduced_lunch",
                    "school_fsar", "school_fsar_imm", "flu_days", 
                    "flu_season_absences_imm", "flu_season_absences_non_imm",
                    "school_fsar_non_imm")
  school <- school[,good_columns]
  

  # Clean up columns
  names(school) <- gsub("[.]x$|school_", "", names(school))
  
  # Make model
  school <- school[which(school$year >= 2011 &
                           school$year <= 2013),]
  

  
  fit <- lm(fsar ~ immRate +  percent_free_reduced_lunch ,
            data = school)
  
  # Predict
  fake <- school[which(school$year == 2013 & school$pubPriv == "pub"),]
  fake <- fake[,c("id", "school", "year", "percent_free_reduced_lunch", "type", "totMem",
                  "immRate")]
  fake <- fake[which(fake$school != "PK Yonge"),]
  
  original <- fake
  original$predicted <- predict(fit, newdata = original)
  # total!
  original_ab <- reactive({
    sum(original$predicted * original$totMem * input$flu_days, na.rm = TRUE)
  })
  
  # baseline
  baseline <- fake
  baseline$immRate <- 0.15
  baseline$predicted <- predict(fit, newdata = baseline)
  baseline_ab <- reactive({
    sum(baseline$predicted * baseline$totMem * input$flu_days, na.rm = TRUE)})
  
  pred <- reactive({
    fake$immRate <- input$ir / 100
    fake$predicted <- predict(fit, newdata = fake)
    fake
    
  })

  #total_ab2 <- 0
  new_ab <- reactive({
    sum(pred()$predicted * pred()$totMem * input$flu_days, na.rm = TRUE)

  })
  
  
  
  ########
  mydata <- reactive({
    
    if(input$type == "all"){
      ir
    } else {
      ir[which(ir$type == input$type),]
    }
    
    })
  
  #######

  ########

  #######
  output$plot1 <- renderPlot({

    dat <- pred()
    #dat <- dat[order(dat$predicted),]
    #dat <- dat[-nrow(dat),]
    
    #baseline <- baseline[order(dat$school),]
    
    par(mar = c(6,4,2,1))
    par(oma = c(6,1,1,1))
    
    bp <- barplot(dat$predicted, ylim = c(0, 0.14),
            names.arg = dat$school,
            las = 3,
            cex.names = 0.7,
            ylab = "Flu season absenteeism rate")
    barplot(baseline$predicted,
            add = TRUE,
            col = adjustcolor("blue", alpha.f = 0.3))
    
    legend("topleft",
           fill = adjustcolor("blue", alpha.f = 0.6),
           legend = "Current level")
    
    text(x = max(bp[,1]) * 0.8,
         y = 0.12,
         labels = paste0(
           "Flu season absences (real): ",
           round(original_ab(), digits = -3), 
           "\n", 
           "Flu season absences (predicted): ",
           round(new_ab(), digits = -3),
         "\n",
         "Total reduction from current levels: ",
         round(original_ab() - new_ab(), digits = -3)))
    
    title(main = paste0(
           "Baseline predicted absences (15% immunization): ",
           round(baseline_ab(), digits = -3),
           "\n",
           "Total reduction from baseline levels: ",
           round(baseline_ab() - new_ab(), digits = -3)
           ), line = 0)
    
    box("plot")
    })
  



  

  ########
  output$text1 <- renderText({
    paste("Predicted absenteeism rates at ", input$ir, "immunization")
  })
  
  ########
  output$text2 <- renderText({
    paste("Team", input$type, "consent form return rate overview")
  })
  

  ########
  output$table1 <- renderDataTable({
    
    x <- mydata()
    x[which(grepl(tolower(input$filter), tolower(mydata()$school))),]
    
  })
  
  ########
  output$motionchart1 <- renderGvis({
    
    gvisMotionChart(data = mydata(), 
                    idvar = "school", 
                    timevar = "year",
                    xvar = "frLunch13",
                    yvar = "immRate",
                    colorvar = "gradeRange",
                    sizevar = "totMem")

  })
  
  

})