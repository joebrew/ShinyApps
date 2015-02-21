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
  
#   # Plot immunization direct and indirect
#   x <- (school$immRate * 100)
#   y <- (school$fsar_imm * 100) 
#   w <- school$totMem
#   par(mfrow = c(1,2))
#   plot(x, y,
#        xlab = "Immunization rate (%)",
#        ylab = "Flu season absenteeism rate (%)",
#        col = adjustcolor("darkblue", alpha.f = 0.6),
#        pch = 16, cex = school$totMem ^(1/2) / 10,
#        ylim = c(0,20))
#   fit <- lm(y~x)
#   abline(fit, col = adjustcolor("darkblue", alpha.f = 0.6), lwd = 3)
#   title(main = "Immunized students")
#   
#   y <- (school$fsar_non_imm * 100) 
#   plot(x, y,
#        xlab = "Immunization rate (%)",
#        ylab = "Flu season absenteeism rate (%)",
#        col = adjustcolor("darkred", alpha.f = 0.6),
#        pch = 16, cex = school$totMem ^(1/2) / 10,
#        ylim = c(0,20))
#   fit <- lm(y~x)
#   abline(fit, col = adjustcolor("darkred", alpha.f = 0.6), lwd = 3)
#   title(main = "Non-immunized students")
#   par(mfrow = c(1,1))
#   
#   
  
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

# MODEL
# ##############
nf <- data.frame("totMem" = sum(fake$totMem),
                 "immRate" = seq(1,100,1)/100,
                 "percent_free_reduced_lunch" = 
                   weighted.mean(fake$percent_free_reduced_lunch, fake$totMem, na.rm = TRUE)
                 )
nf$predicted <- predict(fit, newdata = nf)
nf2 <- reactive({
  nf2 <- nf
  nf2$absences <- nf2$predicted * nf2$totMem * input$flu_days
  #nf2$immRate <- input$ir / 100
  nf2
  
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
output$plot3 <- renderPlot({
  plot(nf2()$immRate*100, nf2()$absences, type = "l",
       col = adjustcolor("darkblue", alpha.f = 0.6),
       lwd = 3,
       xlab = "Immunization rate",
       ylab = "Number of absent student days")
  abline(v = input$ir, col = "red")
  points(x = input$ir,
         y = nf2()$absences[input$ir],
         pch = 16,
         col = "red",
         cex = 2)
  abline(h = seq(0, 200000, 20000),
         col = adjustcolor("black", alpha.f = 0.2))
  abline(v = seq(0,100, 10),
         col = adjustcolor("black", alpha.f = 0.2))
  text(x = 20,
       y = 30000,
       labels = paste0(
         "Esimated flu season absences: ",
         round(nf2()$absences[input$ir],
               digits = -1))
       )
  title(main = "Flu season absent-days")
})

###########

output$plot2 <- renderPlot({
  
  dat <- pred()
  plot(pred()$totMem, pred()$predicted,
       ylim = c(0, 0.1),
       xlab = "Numer of students",
       ylab = "Flu season absenteeism rate",
       col = adjustcolor("darkorange", alpha.f = 0.6),
       cex = (pred()$percent_free_reduced_lunch/100) * 3,
       pch = 16)

  
  abline(h = seq(0, 1, 0.02),
         col = adjustcolor("black", alpha.f = 0.2))
  abline(v = seq(0,20000, 250),
         col = adjustcolor("black", alpha.f = 0.2))
  legend("topright",
         pch = 16,
         col = adjustcolor("darkorange", alpha.f = 0.6),
         legend = c(20, 40, 60, 80),
         pt.cex = (c(20, 40, 60, 80)/100) * 3,
         title = "Percent free lunch")
  
  title(main = "Flu season absenteeism rate")
  
})



output$plot4 <- renderPlot({
  cols <- adjustcolor(colorRampPalette(c("yellow", "blue"))(100), alpha.f = 0.6)
  x <- mydata()
  good_id <- x$id[which(grepl(tolower(input$filter), tolower(mydata()$school)))][1]
  x <- x[which(x$id == good_id),]
  x <- x[c('school', 'immRate', 'year', 'type', 'pubPriv', 
           'percent_free_reduced_lunch', 'id')]
  #x <- x[which(x$id == x$id[1]),]
  x <- x[order(x$year),]
  bp <- barplot(x$immRate * 100, 
          names.arg = x$year,
          ylim = c(0, 75),
          col = cols[round(x$immRate * 100)])
  text(x = bp[,1],
       y = x$immRate * 100,
       pos = 3,
       labels = paste0(round(x$immRate * 100, digits = 1),"%"), )
  title(main = x$school[1])
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
    x <- x[which(grepl(tolower(input$filter), tolower(mydata()$school))),]
    x <- x[c('school', 'immRate', 'year', 'type', 'pubPriv', 
           'percent_free_reduced_lunch')]
  })
  
  ########
  output$motionchart1 <- renderGvis({
    
    gvisMotionChart(data = mydata(), 
                    idvar = "school", 
                    timevar = "year",
                    xvar = "percent_free_reduced_lunch",
                    yvar = "immRate",
                    colorvar = "gradeRange",
                    sizevar = "totMem")
  })
})

# 
# ######
# # TOTAL ABSENCES GIF
# nf$absences <- as.integer(nf$predicted * nf$totMem * 43)
# for (i in seq(1,100,1)){
#   plotname <- as.character(i)
#   while(nchar(plotname) < 5){
#     plotname <- paste0("0", plotname)
#   }
#   png(paste0(plotname, ".png"))
#   plot(nf$immRate*100, nf$absences, type = "l",
#        col = adjustcolor("darkblue", alpha.f = 0.6),
#        lwd = 3,
#        xlab = "Immunization rate",
#        ylab = "Number of absent student days")
#   abline(v = i, col = "red")
#   points(x = i,
#          y = nf$absences[i],
#          pch = 16,
#          col = "red",
#          cex = 2)
#   abline(h = seq(0, 200000, 20000),
#          col = adjustcolor("black", alpha.f = 0.2))
#   abline(v = seq(0,100, 10),
#          col = adjustcolor("black", alpha.f = 0.2))
#   text(x = 20,
#        y = 30000,
#        labels = paste0(
#          "Esimated flu\nseason absences:\n",
#          nf$absences[i]), cex = 1.5)
#   text(x = 70,
#        y = 80000,
#        labels = paste0("Immunization rate:\n",
#                        i, "%"),
#        pos = 3,
#        cex = 2)
#   title(main = "Flu season absent-days")
#   dev.off()
# }



# ##############
# # GIF
# fake <- school[which(school$year == 2013 & school$pubPriv == "pub"),]
# fake <- fake[,c("id", "school", "year", "percent_free_reduced_lunch", "type", "totMem",
#                 "immRate")]
# fake <- fake[which(fake$school != "PK Yonge"),]
# 
# temp <- '/home/joebrew/Desktop/temp'
# setwd(temp)
# for (i in seq(0, 100,1)){
#   filename <- as.character(i)
#   while(nchar(filename) < 3){
#     filename <- paste0("0", filename)
#   }
#   
#   
#   png(paste0("file", filename, ".png"),
#       height = 480, width = 1000)
#   
#   j <- i / 100
#   # first just get all others up to i
#   fake$immRate[which(fake$immRate < j)] <- j
#   fake$p <- predict(fit, newdata = fake)
#   par(mfrow = c(1,2))
#   plot(fake$totMem, fake$p,
#        ylim = c(0, 0.1),
#        xlab = "Numer of students",
#        ylab = "Flu season absenteeism rate",
#        col = adjustcolor("darkorange", alpha.f = 0.6),
#        cex = (fake$percent_free_reduced_lunch/100) * 3,
#        pch = 16)
#   
#   legend("topright",
#          pch = 16,
#          col = adjustcolor("darkorange", alpha.f = 0.6),
#          legend = c(20, 40, 60, 80),
#          pt.cex = (c(20, 40, 60, 80)/100) * 3,
#          title = "Percent free lunch")
#   title(main = "Flu season absenteeism rate")
#   
#   plot(fake$percent_free_reduced_lunch, fake$p * 43 * fake$totMem,
#        ylim = c(0,6000),
#        xlab = "Percent free /reduced lunch",
#        ylab = "Flu season absent days",
#        col = adjustcolor("darkorange", alpha.f = 0.6),
#        cex = (fake$percent_free_reduced_lunch/100) * 3,
#        pch = 16)
#   title(main = "Flu season absent days")
#   
#   par(mfrow = c(1,1))
#   
#   title(main = paste0("Bring all schools up to at least ",
#                       j*100, "%"), outer = TRUE, line = -1)
#   
#   
#   dev.off()
# }

#   #######
#   output$plot1 <- renderPlot({
# 
#     dat <- pred()
#     #dat <- dat[order(dat$predicted),]
#     #dat <- dat[-nrow(dat),]
#     
#     #baseline <- baseline[order(dat$school),]
#     
#     par(mar = c(6,4,2,1))
#     par(oma = c(6,1,1,1))
#     
#     bp <- barplot(baseline$predicted, ylim = c(0, 0.14),
#             names.arg = dat$school,
#             las = 3,
#             cex.names = 0.7,
#             ylab = "Flu season absenteeism rate")
#     barplot(dat$predicted,
#             add = TRUE,
#             col = adjustcolor("blue", alpha.f = 0.3))
#     
#     legend("topleft",
#            fill = adjustcolor(c("grey", "blue"), alpha.f = 0.6),
#            legend = c("At Florida rates", "Current level"))
#     
# #     text(x = max(bp[,1]) * 0.8,
# #          y = 0.12,
# #          labels = paste0(
# #            "Flu season absences (real): ",
# #            round(original_ab(), digits = -3), 
# #            "\n", 
# #            "Flu season absences (predicted): ",
# #            round(new_ab(), digits = -3),
# #          "\n",
# #          "Total reduction from current levels: ",
# #          round(original_ab() - new_ab(), digits = -3)))
#     
#     title(main = paste0(
#            "Baseline predicted absences (15% immunization): ",
#            round(baseline_ab(), digits = -3),
#            "\n",
#            "Total reduction from baseline levels: ",
#            round(baseline_ab() - new_ab(), digits = -3)
#            ), line = 0)
#     
#     box("plot")
#     })
