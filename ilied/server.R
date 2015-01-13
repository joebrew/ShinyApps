# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(googleVis)
#setwd("C:/Users/BrewJR/Documents/healthy_schools/shiny/healthyschools")
source("boselli_tallahassee_presentation_january_2015.R")
source("sliv_model.R")


shinyServer(function(input, output) {
  
  output$plot1 <- renderPlot({

    par(mfrow = c(1,2))
    par(mar = c(6, 2, 2, 1))
    # Direct costs
    visualize(fun = direct_costs, new_imm = input$new_imm, current_imm = input$current_imm,
              show_difference = input$show_difference)
    title(main = "Direct costs", cex.main = 1.5)
    
    #medicaid costs
    visualize(fun = medicaid_costs, new_imm = input$new_imm, current_imm = input$current_imm,
              show_difference = input$show_difference)
    title(main = "Medicaid costs", cex.main = 1.5)
    

    
    par(mfrow = c(1,1))
  })
  
  output$plot2 <- renderPlot({
    
    par(mfrow = c(1,2))
    par(mar = c(6, 2, 2, 1))
    
    #cases
    visualize(fun = cases, new_imm = input$new_imm, current_imm = input$current_imm,
              show_difference = input$show_difference)
    title(main = "Cases", cex.main = 1.5)
    
    #hospitalizations
    visualize(fun = hospitalizations, new_imm = input$new_imm, current_imm = input$current_imm,
              show_difference = input$show_difference)
    title(main = "Hospitalizations", cex.main = 1.5)
    
    par(mfrow = c(1,1))
  })
  
  output$plot3 <- renderPlot({
    
    par(mfrow = c(1,2))
    par(mar = c(6, 2, 2, 1))
    

    
    #indirect costs
    visualize(fun = indirect_costs, new_imm = input$new_imm, current_imm = input$current_imm,
              show_difference = input$show_difference)
    title(main = "Indirect costs", cex.main = 1.5)
    
    #deaths
    visualize(fun = deaths, new_imm = input$new_imm, current_imm = input$current_imm,
              show_difference = input$show_difference)
    title(main = "Deaths", cex.main = 1.5)
    
    
    
    par(mfrow = c(1,1))
  })
  
  output$plot4 <- renderPlot({
    

    
    if(!input$show_alachua){
      plot_scenario(df = real,
                    county = input$county,
                    add = FALSE,
                    color = "blue")
    
    } else{
      
      plot_scenario(df = real,
                    county = input$county,
                    add = FALSE,
                    color = "blue")
      plot_scenario(df = best,
                    county = input$county,
                    add = TRUE,
                    color = "red")
      

      
    }
    
    title(main = "ILI ED cases")

  })
  
  output$plot5 <- renderPlot({
    
    ed_savings(df = real, 
               county = input$county, 
               alachua_row=TRUE, 
               add = FALSE, 
               color = "blue",
               ed_cost = input$ed_cost,
               show_alachua = input$show_alachua)
    
  })
  
  
  output$bill1 <- renderPlot({
    
    #myMap <- map("county", "florida", plot = FALSE)
    par(mfrow=c(1,2))
    
    if(input$var == "ENTIRE STATE"){
      myMap$color  <- rep("blue", length(myMap$county))
    } else{
      myMap$color <- ifelse(myMap$county == input$var,
                            "blue",
                            "grey")
    }
    
    
    map("county", "florida", border="darkgrey", fill=TRUE, col=myMap$color)
    
    
    if(input$var == "ENTIRE STATE"){
      tempSchool <- dfSchool
    } else{
      tempSchool <- dfSchool[which(dfSchool$district == input$var),]
    }
    
    
    
    red <- round(CostFun(totMem = sum(tempSchool$totMem, na.rm=TRUE), 
                         vfcPer = weighted.mean(tempSchool$vfcPer,
                                                tempSchool$totMem,
                                                na.rm=TRUE),
                         immRate = input$immRate,
                         privateVacCost = 17.5,
                         delivCost = .05,
                         billCost = 5.23,
                         printCost = .1,
                         storageCost = .05,
                         nursePerHour = 25,
                         randp=input$randp), 
                 digits=0)
    green <- round(RevFun(totMem = sum(tempSchool$totMem, na.rm=TRUE), 
                          vfcPer = weighted.mean(tempSchool$vfcPer,
                                                 tempSchool$totMem,
                                                 na.rm=TRUE),
                          immRate = input$immRate,
                          privDenRate = 100 - input$sucBill,
                          privUnbillableRate = 5.123104,
                          vfcDenRate = 20,
                          privAvgRe = 39.39,
                          vfcAvgRe = 5), 
                   digits=0)
    bp_elements <- c(red, green)
    bp <- barplot(bp_elements, 
                  ylim = c(0, max(bp_elements)*1.1),
                  cex.lab = 0.6,
                  cex.axis = 0.6,
                  names.arg=c("Costs", "Revenue"),
                  border="darkgrey",
                  col=adjustcolor(c("darkred", "darkgreen"), alpha.f = 0.6)) 
    abline(h=seq(0, max(bp_elements), length = 5), 
           col = adjustcolor("black", alpha.f = 0.2),
           lty = 2)
    box("plot")
    text(x = bp[,1],
         y = bp_elements,
         pos = 1,
         labels = paste0("$", round(bp_elements)),
         cex = 0.55,
         col = adjustcolor("black", alpha.f = 0.6))
    rev <- abs(green - red)
    if(green - red >= 0){
      rev_label <- "Net program revenue\nof "
    } else{
      rev_label <- "Net program cost\nof "
    }
    lines(x = bp[,1],
          y = bp_elements,
          col = adjustcolor("black", alpha.f = 0.4))
    points(x = bp[,1],
           y = bp_elements,
           col = adjustcolor("black", alpha.f = 0.4),
           pch = 16)
    text(x = mean(bp[,1]),
         y = max(bp_elements)*1.05,
         labels = paste0(rev_label, " $", rev),
         col = adjustcolor("black", alpha.f = 0.6),
         cex = 0.7)
  })
  
  output$bill2 <- renderPlot({
    
    par(mfrow=c(1,2))
    par(mar=c(5,3,5,3))
    par(oma=c(0,0,0,0))
    
    if(input$var == "ENTIRE STATE"){
      tempSchool <- dfSchool
    } else{
      tempSchool <- dfSchool[which(dfSchool$district == input$var),]
    }
    
    red <- VacFun(totMem = sum(tempSchool$totMem, na.rm=TRUE), 
                  vfcPer = weighted.mean(tempSchool$vfcPer,
                                         tempSchool$totMem,
                                         na.rm=TRUE),
                  immRate = input$immRate,
                  privateVacCost = 17.5)
    green <- OtherCostFun(totMem = sum(tempSchool$totMem, na.rm=TRUE), 
                          vfcPer = weighted.mean(tempSchool$vfcPer,
                                                 tempSchool$totMem,
                                                 na.rm=TRUE),
                          immRate = input$immRate,
                          delivCost = .05,
                          billCost = 5.23,
                          printCost = .1,
                          storageCost = .05,
                          nursePerHour = 25,
                          randp=input$randp)
    bp_elements <- c(red, green)
    
    bp <- barplot(bp_elements,
                  names.arg=c("Vaccine purchase", 
                              "Other costs"),
                  main="Program costs",
                  ylim = c(0, max(bp_elements)*1.1),
                  col = adjustcolor("darkred", alpha.f = 0.6))
    
    text(x = bp[,1],
         y = bp_elements,
         pos = 1,
         col = adjustcolor("black", alpha.f = 0.6),
         labels = paste0("$", round(bp_elements, digits = -2)))
    text(x = bp[,1],
         y = bp_elements,
         pos = 3,
         col = adjustcolor("black", alpha.f = 0.6),
         labels = paste0(round(bp_elements/sum(bp_elements)*100), "%"))
    
    abline(h=seq(0, max(bp_elements), length = 5), 
           col = adjustcolor("black", alpha.f = 0.2),
           lty = 2)
    
    box("plot")
    
    
    #REVENUE
    red <- privateRevFun(totMem = sum(tempSchool$totMem, na.rm=TRUE), 
                         vfcPer = weighted.mean(tempSchool$vfcPer,
                                                tempSchool$totMem,
                                                na.rm=TRUE),
                         immRate = input$immRate,
                         privDenRate = 100 - input$sucBill,
                         privUnbillableRate = 5.123104,
                         vfcDenRate = 20,
                         privAvgRe = 39.39,
                         vfcAvgRe = 5)
    green <- vfcRevFun(totMem = sum(tempSchool$totMem, na.rm=TRUE), 
                       vfcPer = weighted.mean(tempSchool$vfcPer,
                                              tempSchool$totMem,
                                              na.rm=TRUE),
                       immRate = input$immRate,
                       privDenRate = 100 - input$sucBill,
                       privUnbillableRate = 5.123104,
                       vfcDenRate = 20,
                       privAvgRe = 39.39,
                       vfcAvgRe = 5)
    bp_elements <- c(red, green)
    bp <- barplot(bp_elements,
                  names.arg=c("Billing\nprivate insurers",
                              "Billing\nfor VFC"),
                  main="Revenue sources",
                  ylim = c(0, max(bp_elements)*1.1),
                  col = adjustcolor("darkgreen", alpha.f = 0.6))
    
    text(x = bp[,1],
         y = bp_elements,
         pos = 1,
         col = adjustcolor("black", alpha.f = 0.6),
         labels = paste0("$", round(bp_elements, digits = -2)))
    text(x = bp[,1],
         y = bp_elements,
         pos = 3,
         col = adjustcolor("black", alpha.f = 0.6),
         labels = paste0(round(bp_elements/sum(bp_elements)*100), "%"))
    
    abline(h=seq(0, max(bp_elements), length = 5), 
           col = adjustcolor("black", alpha.f = 0.2),
           lty = 2)
    
    box("plot")
    
    
  })
  
  
  


#   
#   output$motionchart1 <- renderGvis({
#     
#     gvisMotionChart(data = WorldBank,
#                     idvar="country", timevar="year",
#                     xvar="life.expectancy", 
#                     yvar="fertility.rate",
#                     colorvar="region", 
#                     sizevar="population",
#                     options=list(width=550, height=500))
#     
#   })
  
})