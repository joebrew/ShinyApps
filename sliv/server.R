# server.R

# turn off scientific notation
options(scipen=999)

#load in packages
library(maps)
library(classInt)
library(dplyr)
#library(RCurl)
library(xtable)
library(RColorBrewer)

#source the model file
#source("ShinyApps/sliv/model.R")
source("model.R")


shinyServer(
  function(input, output) {
    
    
    

    
    
    output$text2 <- renderText({ 
      
      if(input$var == "ENTIRE STATE"){
        tempSchool <- dfSchool
      } else{
        tempSchool <- dfSchool[which(dfSchool$district == input$var),]
      }
      

      
      paste("Program costs: $",
            round(CostFun(totMem = sum(tempSchool$totMem, na.rm=TRUE), 
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
                  digits=2)
            
      )
    })
    
    output$text3 <- renderText({ 
      
      if(input$var == "ENTIRE STATE"){
        tempSchool <- dfSchool
      } else{
        tempSchool <- dfSchool[which(dfSchool$district == input$var),]
      }
      
      
      paste("Program gross revenue: $",
            round(RevFun(totMem = sum(tempSchool$totMem, na.rm=TRUE), 
                         vfcPer = weighted.mean(tempSchool$vfcPer,
                                                tempSchool$totMem,
                                                na.rm=TRUE),
                         immRate = input$immRate,
                         privDenRate = 100 - input$sucBill,
                         privUnbillableRate = 5.123104,
                         vfcDenRate = 20,
                         privAvgRe = 39.39,
                         vfcAvgRe = 5), 
                  digits=2)
            
      )
    })
    
    output$text4 <- renderText({ 
      
      if(input$var == "ENTIRE STATE"){
        tempSchool <- dfSchool
      } else{
        tempSchool <- dfSchool[which(dfSchool$district == input$var),]
      }
      
      paste("Net surplus/deficit: $",
            round(round(RevFun(totMem = sum(tempSchool$totMem, na.rm=TRUE), 
                               vfcPer = weighted.mean(tempSchool$vfcPer,
                                                      tempSchool$totMem,
                                                      na.rm=TRUE),
                               immRate = input$immRate,
                               privDenRate = 100 - input$sucBill,
                               privUnbillableRate = 5.123104,
                               vfcDenRate = 20,
                               privAvgRe = 39.39,
                               vfcAvgRe = 5), 
                        digits=2) - 
                    round(CostFun(totMem = sum(tempSchool$totMem, na.rm=TRUE), 
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
                          digits=2), digits=2)
            
      )
    })
    
    output$text5 <- renderText({ 
      
      if(input$var == "ENTIRE STATE"){
        tempSchool <- dfSchool
      } else{
        tempSchool <- dfSchool[which(dfSchool$district == input$var),]
      }
      
      
      paste(
        "Number of students:",
        sum(tempSchool$totMem, na.rm=TRUE)
      )
    })
    
    output$text6 <- renderText({ 
      
      if(input$var == "ENTIRE STATE"){
        tempSchool <- dfSchool
      } else{
        tempSchool <- dfSchool[which(dfSchool$district == input$var),]
      }
      
      paste(
        "Percent with private health insurance:",
        paste(round(100 - weighted.mean(tempSchool$vfcPer,
                                        tempSchool$totMem, 
                                        na.rm=TRUE), digits=2)),
        "%"
        
      )
    })
    
    
    output$text6b <- renderText({ 
      
      if(input$var == "ENTIRE STATE"){
        tempSchool <- dfSchool
      } else{
        tempSchool <- dfSchool[which(dfSchool$district == input$var),]
      }
      
      
      paste(
        "Number of schools:",
        nrow(tempSchool)
        
      )
    })
    
    
    output$text7 <- renderText({ 
      
      if(input$var == "ENTIRE STATE"){
        tempSchool <- dfSchool
      } else{
        tempSchool <- dfSchool[which(dfSchool$district == input$var),]
      }
      
      
      paste(
        "Vaccine purchase cost: $",
        
        round(VacFun(totMem = sum(tempSchool$totMem, na.rm=TRUE), 
                     vfcPer = weighted.mean(tempSchool$vfcPer,
                                            tempSchool$totMem,
                                            na.rm=TRUE),
                     immRate = input$immRate,
                     privateVacCost = 17.5), 
              digits=2)
        
      )
    })
    
    
    
    
    output$text8 <- renderText({ 
      
      if(input$var == "ENTIRE STATE"){
        tempSchool <- dfSchool
      } else{
        tempSchool <- dfSchool[which(dfSchool$district == input$var),]
      }
      
      
      paste(
        "Other program costs: $",
        
        round(OtherCostFun(totMem = sum(tempSchool$totMem, na.rm=TRUE), 
                           vfcPer = weighted.mean(tempSchool$vfcPer,
                                                  tempSchool$totMem,
                                                  na.rm=TRUE),
                           immRate = input$immRate,
                           delivCost = .05,
                           billCost = 5.23,
                           printCost = .1,
                           storageCost = .05,
                           nursePerHour = 25,
                           randp=input$randp), 
              digits=2)
        
        
      )
    })
    
    
    
    
    output$text8b <- renderText({ 
      
      if(input$var == "ENTIRE STATE"){
        tempSchool <- dfSchool
      } else{
        tempSchool <- dfSchool[which(dfSchool$district == input$var),]
      }
      
      paste(
        "Total costs: $",
        
        round(CostFun(totMem = sum(tempSchool$totMem, na.rm=TRUE), 
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
              digits=2)
        
      )
    })
    
    
    output$text9 <- renderText({ 
      
      if(input$var == "ENTIRE STATE"){
        tempSchool <- dfSchool
      } else{
        tempSchool <- dfSchool[which(dfSchool$district == input$var),]
      }
      
      paste("Revenue from billing private insurers: $",
            round(privateRevFun(totMem = sum(tempSchool$totMem, na.rm=TRUE), 
                                vfcPer = weighted.mean(tempSchool$vfcPer,
                                                       tempSchool$totMem,
                                                       na.rm=TRUE),
                                immRate = input$immRate,
                                privDenRate = 100 - input$sucBill,
                                privUnbillableRate = 5.123104,
                                vfcDenRate = 20,
                                privAvgRe = 39.39,
                                vfcAvgRe = 5), 
                  digits=2))
      
    })
    
    output$text10 <- renderText({ 
      
      if(input$var == "ENTIRE STATE"){
        tempSchool <- dfSchool
      } else{
        tempSchool <- dfSchool[which(dfSchool$district == input$var),]
      }
      
      paste("Revenue for VFC administration: $",
            round(vfcRevFun(totMem = sum(tempSchool$totMem, na.rm=TRUE), 
                            vfcPer = weighted.mean(tempSchool$vfcPer,
                                                   tempSchool$totMem,
                                                   na.rm=TRUE),
                            immRate = input$immRate,
                            privDenRate = 100 - input$sucBill,
                            privUnbillableRate = 5.123104,
                            vfcDenRate = 20,
                            privAvgRe = 39.39,
                            vfcAvgRe = 5), 
                  digits=2))
    })
    
    
    output$text11 <- renderText({ 
      
      if(input$var == "ENTIRE STATE"){
        tempSchool <- dfSchool
      } else{
        tempSchool <- dfSchool[which(dfSchool$district == input$var),]
      }
      
      paste("Total gross revenue: $",
            round(RevFun(totMem = sum(tempSchool$totMem, na.rm=TRUE), 
                         vfcPer = weighted.mean(tempSchool$vfcPer,
                                                tempSchool$totMem,
                                                na.rm=TRUE),
                         immRate = input$immRate,
                         privDenRate = 100 - input$sucBill,
                         privUnbillableRate = 5.123104,
                         vfcDenRate = 20,
                         privAvgRe = 39.39,
                         vfcAvgRe = 5), 
                  digits=2))
    })
    
    
    output$text12 <- renderText({ 
      paste("The following is interpolated from",
            "Weycker D, Edelsberg J, Halloran ME, et al.",
            "Population-wide benefits of routine vaccination of children against influenza.",
            "Vaccine 2005; 23(10): 1284-93.")
    })
    
    output$text13 <- renderText({ 
      paste("(Figures have been adjusted for inflation and population growth, 
            and assume a 5% non-SLIV pediatric immunization rate)")
    })
    
    output$text14 <- renderText({ 
      paste(" ")
    })
    
    
    
    
    
    
    
    output$plot1 <- renderPlot({
      
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
    
    
    
    output$plot2 <- renderPlot({
      
      par(mfrow=c(1,2))
      
      if(input$var == "ENTIRE STATE"){
        tempfl <- fl
      } else{
        tempfl <- fl[which(fl$county == input$var),]
      }
      
      
      
      #CASES AVERTED
      plot(1:2, 1:2,
           xlim=c(0,100),
           ylim=c(0,max(tempfl$cases)),
           type="n",
           xlab="SLIV Immunization rate",
           ylab="Influenza cases (annual)")
      
      for (i in 1:100){
        points(x = i,
               y = max(tempfl$cases)  - CasesFun(county = input$var, 
                                                 immRate = i),
               pch=16,
               cex=0.5,
               col=adjustcolor("black", alpha.f=0.5))
      }
      
      points(x = input$immRate,
             y = max(fl$cases[which(fl$county == input$var)])  - CasesFun(county = input$var, 
                                                                          immRate = input$immRate),
             pch=16,
             cex=2,
             col="red")
      
      abline(h = max(fl$cases[which(fl$county == input$var)])  - CasesFun(county = input$var, 
                                                                          immRate = input$immRate),
             col=adjustcolor("black", alpha.f=0.2))
      
      abline(v=input$immRate,
             col=adjustcolor("black", alpha.f=0.2))
      
      text(x = ifelse(input$immRate < 80,
                      input$immRate + 10,
                      input$immRate - 10),
           y = ifelse(input$immRate < 20,
                      (max(fl$cases[which(fl$county == input$var)])  - CasesFun(county = input$var, 
                                                                                immRate = input$immRate)) *0.75,
                      (max(fl$cases[which(fl$county == input$var)])  - CasesFun(county = input$var, 
                                                                                immRate = input$immRate)) *1.5),
           labels=paste0(round(CasesFun(county = input$var, 
                                        immRate = input$immRate), digits=0),
                         " cases\n averted"))
      
      title(main="Cases")
      
      
      #LIVES SAVED
      ##### 
      plot(1:2, 1:2,
           xlim=c(0,100),
           ylim=c(0,max(fl$deaths[which(fl$county == input$var)])),
           type="n",
           xlab="SLIV Immunization rate",
           ylab="Influenza deaths (annual)")
      
      for (i in 1:100){
        points(x = i,
               y = max(fl$deaths[which(fl$county == input$var)])  - DeathsFun(county = input$var, 
                                                                              immRate = i),
               pch=16,
               cex=0.5,
               col=adjustcolor("black", alpha.f=0.5))
      }
      
      points(x = input$immRate,
             y = max(fl$deaths[which(fl$county == input$var)])  - DeathsFun(county = input$var, 
                                                                            immRate = input$immRate),
             pch=16,
             cex=2,
             col="red")
      
      abline(h = max(fl$deaths[which(fl$county == input$var)])  - DeathsFun(county = input$var, 
                                                                            immRate = input$immRate),
             col=adjustcolor("black", alpha.f=0.2))
      
      abline(v=input$immRate,
             col=adjustcolor("black", alpha.f=0.2))
      
      text(x = ifelse(input$immRate < 80,
                      input$immRate + 10,
                      input$immRate - 10),
           y = ifelse(input$immRate < 20,
                      (max(fl$deaths[which(fl$county == input$var)])  - DeathsFun(county = input$var, 
                                                                                  immRate = input$immRate)) *0.75,
                      (max(fl$deaths[which(fl$county == input$var)])  - DeathsFun(county = input$var, 
                                                                                  immRate = input$immRate)) *1.5),
           labels=paste0(round(DeathsFun(county = input$var, 
                                         immRate = input$immRate), digits=0),
                         " lives \n saved"))
      
      title(main="Deaths")
      
      title(main=input$var, outer=TRUE, line=-1)
      
      
    })
    
    
    output$plot3 <- renderPlot({
      
      par(mfrow= c(1,2))
      
      #hospitalizations
      ##### 
      plot(1:2, 1:2,
           xlim=c(0,100),
           ylim=c(0,max(fl$hospitalizations[which(fl$county == input$var)])),
           type="n",
           xlab="SLIV Immunization rate",
           ylab="Influenza hospitalizations (annual)")
      
      for (i in 1:100){
        points(x = i,
               y = max(fl$hospitalizations[which(fl$county == input$var)])  - HospitalizationsFun(county = input$var, 
                                                                                                  immRate = i),
               pch=16,
               cex=0.5,
               col=adjustcolor("black", alpha.f=0.5))
      }
      
      points(x = input$immRate,
             y = max(fl$hospitalizations[which(fl$county == input$var)])  - HospitalizationsFun(county = input$var, 
                                                                                                immRate = input$immRate),
             pch=16,
             cex=2,
             col="red")
      
      abline(h = max(fl$hospitalizations[which(fl$county == input$var)])  - HospitalizationsFun(county = input$var, 
                                                                                                immRate = input$immRate),
             col=adjustcolor("black", alpha.f=0.2))
      
      abline(v=input$immRate,
             col=adjustcolor("black", alpha.f=0.2))
      
      text(x =ifelse(input$immRate < 80,
                     input$immRate + 10,
                     input$immRate - 10),
           y = ifelse(input$immRate < 20,
                      (max(fl$hospitalizations[which(fl$county == input$var)])  - HospitalizationsFun(county = input$var, 
                                                                                                      immRate = input$immRate)) *0.75,
                      (max(fl$hospitalizations[which(fl$county == input$var)])  - HospitalizationsFun(county = input$var, 
                                                                                                      immRate = input$immRate)) *1.5),
           labels=paste0(round(HospitalizationsFun(county = input$var, 
                                                   immRate = input$immRate), digits=0),
                         " \n hospitalizations \n averted"))      
      
      title(main="Hospitalizations")
      
      
      
      #COST REDUCTIONS
      
      
      #DIRECT COSTS
      
      ##### 
      plot(1:2, 1:2,
           xlim=c(1,100),
           ylim=c(1,max(fl$indirectCosts[which(fl$county == input$var)])),
           type="n",
           xlab="SLIV Immunization rate",
           ylab="Costs (annual)",
           cex.axis=0.7)
      
      for (i in 1:100){
        points(x = i,
               y = max(fl$directCosts[which(fl$county == input$var)])  - DirectCostsFun(county = input$var, 
                                                                                        immRate = i),
               pch=16,
               cex=0.5,
               col=adjustcolor("red", alpha.f=0.5))
      }
      
      points(x = input$immRate,
             y = max(fl$directCosts[which(fl$county == input$var)])  - DirectCostsFun(county = input$var, 
                                                                                      immRate = input$immRate),
             pch=16,
             cex=2,
             col="red")
      
      abline(h = max(fl$directCosts[which(fl$county == input$var)])  - DirectCostsFun(county = input$var, 
                                                                                      immRate = input$immRate),
             col=adjustcolor("black", alpha.f=0.2))
      
      abline(v=input$immRate,
             col=adjustcolor("black", alpha.f=0.2))
      
      text(x = ifelse(input$immRate < 80,
                      input$immRate + 10,
                      input$immRate - 10),
           y = ifelse(input$immRate < 20,
                      (max(fl$directCosts[which(fl$county == input$var)])  - DirectCostsFun(county = input$var, 
                                                                                            immRate = input$immRate)) *0.75,
                      (max(fl$directCosts[which(fl$county == input$var)])  - DirectCostsFun(county = input$var, 
                                                                                            immRate = input$immRate)) *1.5),
           labels=paste0("$" ,round(DirectCostsFun(county = input$var, 
                                                   immRate = input$immRate)/1000000, digits=2),
                         " million saved \n in direct costs"),
           cex=0.8)
      
      
      #INDIRECT COSTS
      
      for (i in 1:100){
        points(x = i,
               y = max(fl$indirectCosts[which(fl$county == input$var)])  - IndirectCostsFun(county = input$var, 
                                                                                            immRate = i),
               pch=16,
               cex=0.5,
               col=adjustcolor("blue", alpha.f=0.5))
      }
      
      
      points(x = input$immRate,
             y = max(fl$indirectCosts[which(fl$county == input$var)])  - IndirectCostsFun(county = input$var, 
                                                                                          immRate = input$immRate),
             pch=16,
             cex=2,
             col="blue")
      
      text(x = ifelse(input$immRate < 80,
                      input$immRate + 10,
                      input$immRate - 10),
           y = ifelse(input$immRate < 20,
                      (max(fl$indirectCosts[which(fl$county == input$var)])  - IndirectCostsFun(county = input$var, 
                                                                                                immRate = input$immRate)) *0.75,
                      (max(fl$indirectCosts[which(fl$county == input$var)])  - IndirectCostsFun(county = input$var, 
                                                                                                immRate = input$immRate)) *1.5),
           labels=paste0("$" ,round(IndirectCostsFun(county = input$var, 
                                                     immRate = input$immRate)/1000000, digits=2),
                         " million saved \n in indirect costs"),
           cex=0.8)
      
      
      title(main="Costs of flu")
      
    })
    
    
    
    
    
    output$plot4 <- renderPlot({
      
      par(mar=c(6,4,1,1))
      par(oma=c(8,1,1,1))
      
      if(input$var == "ENTIRE STATE"){
        temp <- dfSchool
        temp <- temp %>%
          group_by(district) %>%
          summarise(totMem = sum(totMem, na.rm = TRUE),
                    free = sum(free, na.rm = TRUE),
                    reduced = sum(reduced, na.rm = TRUE))
        # Add the per columns
        temp <- temp %>%
          mutate(frper = (free + reduced) / totMem * 100)
        temp$vfcPer <- temp$frper * (1/1.173212)
        
        # Order
        temp <- temp[order(temp$totMem),]
        
        # Plot
        barplot(temp$totMem,
                yaxt="n",
                xaxt="n",
                names.arg=NA,
                cex.axis = 0.5,
                cex.lab = 0.5,
                ylim = c(0, max(temp$totMem, na.rm = TRUE)*1.1))
        barplot(temp$totMem*temp$vfcPer/100,
                names.arg=temp$district,
                cex.names=ifelse(nrow(temp)>100,
                                 0.1,
                                 0.5),
                las=3,
                add=TRUE,
                col=adjustcolor("darkred", alpha.f=0.4),
                border=NA,
                ylab="Students",
                cex.axis = 0.5,
                cex.lab = 0.5)
        abline(h= seq(0,max(temp$totMem, na.rm = T), length = 5),
               col = adjustcolor("black", alpha.f = 0.2),
               lty = 2)
        
        legend(x="topleft",
               fill=adjustcolor("darkred", alpha.f=0.5),
               border=NA,
               legend="VFC eligible",
               bty="n")
        box("plot")
        
        
      } else {
        temp <- dfSchool[which(dfSchool$district == input$var),]
        temp <- temp[order(temp$totMem),]
        
        barplot(temp$totMem,
                yaxt="n",
                xaxt="n",
                names.arg=NA,
                cex.axis = 0.5,
                cex.lab = 0.5,
                ylim = c(0, max(temp$totMem, na.rm = TRUE)*1.1))
        barplot(temp$totMem*temp$vfcPer/100,
                names.arg=temp$School,
                cex.names=ifelse(nrow(temp)>100,
                                 0.1,
                                 0.5),
                las=3,
                add=TRUE,
                col=adjustcolor("darkred", alpha.f=0.4),
                border=NA,
                ylab="Students",
                cex.axis = 0.5,
                cex.lab = 0.5)
        abline(h= seq(0,max(temp$totMem, na.rm = T), length = 5),
               col = adjustcolor("black", alpha.f = 0.2),
               lty = 2)
        
        legend(x="topleft",
               fill=adjustcolor("darkred", alpha.f=0.5),
               border=NA,
               legend="VFC eligible",
               bty="n")
        box("plot")
      }
      
      
      
      
    })
    
    
    output$plot5 <- renderPlot({
      
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
    
    
    
    
    # Generate an HTML table view of the data
    output$table1 <- renderDataTable({
      
      if(input$var == "ENTIRE STATE"){
        temp <- dfSchool
      } else {
        temp <- dfSchool[which(dfSchool$district == input$var),]
      }
      
      temp <- temp[order(temp$totMem),]
      
      
      temp$Cost <- round(CostFun(totMem = temp$totMem, 
                                     vfcPer = temp$vfcPer,
                                     immRate = input$immRate,
                                     privateVacCost = 17.5,
                                     delivCost = .05,
                                     billCost = 5.23,
                                     printCost = .1,
                                     storageCost = .05,
                                     nursePerHour = 25,
                                     randp=input$randp), 
                             digits=0)
      
      
      temp$Revenue <- round(RevFun(totMem = temp$totMem, 
                                       vfcPer = temp$vfcPer,
                                       immRate = input$immRate,
                                       privDenRate = 100 - input$sucBill,
                                       privUnbillableRate = 5.123104,
                                       vfcDenRate = 20,
                                       privAvgRe = 39.39,
                                       vfcAvgRe = 5), 
                                digits=0)
      
      
      temp$priv <- 100 - temp$vfcPer
      
      
      ##############################
      ### Make temp2
      ##############################
      temp2 <- temp[,c("district",
                               "School",
                               "totMem",
                               "vfcPer",
                               "priv",
                               "Cost",
                               "Revenue")]
      
      temp2$vfcPer <- paste0(round(temp2$vfcPer, digits=1), "%")
      temp2$priv <- paste0(round(temp2$priv, digits=1), "%")
      
      temp2$totMem <- gsub(".00","",as.character(temp2$totMem))
      
      
      colnames(temp2) <-
        c("District", "School", "Students", "Percent VFC", 
          "Percent Private", "Cost",
          "Revenue")
      
      temp2$Net <- round(temp2$Revenue - temp2$Cost, digits=0)
      
      temp2$Revenue <- paste0("$", temp2$Revenue)
      temp2$Cost <- paste0("$", temp2$Cost)
      temp2$Net <- paste0("$", temp2$Net)
      
      
      temp2 <- temp2[order(temp2$District),]
      
      
      
      data.frame(temp2)
    })
    
    
    
    }
)