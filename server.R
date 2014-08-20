# source("./ShinyApps/cdph/helper.R")

# LOAD PACKAGES
library(shiny)
library(maptools)
library(rgdal)
#library(plyr)
library(stringr)
library(Hmisc)
library(SemiPar)
library(sp)
library(RColorBrewer)
library(classInt)
library(gpclib)
gpclibPermit <- TRUE

load("/home/joebrew/ShinyApps/cdph/july7.RData") #maps
load("/home/joebrew/ShinyApps/cdph/cen.df.RData") #census data
load("/home/joebrew/ShinyApps/cdph/ts.RData") # time series made from data
load("/home/joebrew/ShinyApps/cdph/preds.RData") #Subho's model data

preds$Response.Name <- ifelse(preds$Response == 1,
                              "interior hazard",
                              ifelse(preds$Response == 2,
                                     "exterior hazard",
                                     ifelse(preds$Response == 3,
                                            "compliance", 
                                            ifelse(preds$Response == 4,
                                                   "BLL > 5",
                                                   ifelse(preds$Response == 5,
                                                          "BLL > 10", 
                                                          ifelse(preds$Response == 6,
                                                                 "BLL > 20",
                                                                 NA))))))

preds$Model.Name <- ifelse(preds$Model == 1,
                           "logistic regression",
                           ifelse(preds$Model == 2,
                                  "classification tree",
                                  ifelse(preds$Model == 3,
                                         "bagging 10",
                                         ifelse(preds$Model == 4,
                                                "bagging 25",
                                                ifelse(preds$Model == 5,
                                                       "naive Bayes",
                                                       NA)))))

joeNames <- data.frame(c("Mean BLL" ,
                         "Interior hazard rate",
                         "Exterior hazard rate",
                         "Number of tests > 10 ug/dL",
                         "Number of tests > 20 ug/dL", 
                         "Percent > 10 ug/dL" , 
                         "Percent > 20 ug/dL" ,
                         "Median BLL" , 
                         "Total number of tests" , 
                         "Percent poverty" , 
                         "Percent HS graduation",
                         "Percent college graduation",
                         "Percent non-latino white" , 
                         "Percent non-latino black" , 
                         "Percent latino",
                         "Mean age at first test", 
                         "Median age at first test"))

colnames(joeNames) <- c("Label")


joeNames$var <- c("mean.bll",
                  "interiorTF",
                  "exteriorTF",
                  "nTestsOver10",
                  "nTestsOver20",
                  "pTestsOver10",
                  "pTestsOver20",
                  "median.bll",
                  "nTests",
                  "PtPov",
                  "PtHSGED",
                  "PtBAPlus",
                  "PtNLWh",
                  "PtNLB", 
                  "PtL",
                  "meanAgeFirstTest",
                  "medianAgeFirstTest")



#################
# WRITE A PLOTTING FUNCTION FOR VISUALIZING
# MAPS OF TESTS BY CENSUS TRACT
#################
TractFun <- function(var, color, breaks){
  var[is.nan(var)] <- 0
  var[is.infinite(var)] <- 0
  plotvar <- var
  nclr <- breaks
  plotclr <- brewer.pal(nclr, color)
  class <- classIntervals(plotvar, nclr, style = "quantile", dataPrecision=1) #use "equal" instead
  #class <- classIntervals(0:100, nclr, style="equal")
  colcode <- findColours(class, plotclr)
  legcode <- paste0(gsub(",", " - ", gsub("[[]|[]]|[)]", "", names(attr(colcode, "table")))), "")
  plot(cen, border=NA, col=colcode)
  legend("left", # position
         legend = legcode, #names(attr(colcode, "table")), 
         fill = attr(colcode, "palette"), 
         cex = 1.4, 
         border=NA,
         bty = "n")
}

TractFunFixed <- function(var, color, myMax ){
  var[is.nan(var)] <- 0
  var[is.infinite(var)] <- 0
  plotvar <- var
  nclr <- 9
  plotclr <- brewer.pal(nclr, color)
  class <- classIntervals(plotvar, nclr, style = "fixed", 
                          fixedBreaks=round(seq(0, myMax, length=10), digits=2)) 
  #class <- classIntervals(0:100, nclr, style="equal")
  colcode <- findColours(class, plotclr)
  legcode <- paste0(gsub(",", " - ", gsub("[[]|[]]|[)]", "", names(attr(colcode, "table")))), "")
  plot(cen, border=NA, col=colcode)
  legend("left", # position
         legend = legcode, #names(attr(colcode, "table")), 
         fill = attr(colcode, "palette"), 
         cex = 1, 
         border=NA,
         bty = "n")
}

TractFun2 <- function(var, color){
  var[is.nan(var)] <- 0
  var[is.infinite(var)] <- 0
  plotvar <- var
  nclr <- 9
  plotclr <- brewer.pal(nclr, color)
  class <- classIntervals(plotvar, nclr, style = "quantile", dataPrecision=1) #use "equal" instead
  #class <- classIntervals(0:100, nclr, style="equal")
  colcode <- findColours(class, plotclr)
  legcode <- paste0(gsub(",", " - ", gsub("[[]|[]]|[)]", "", names(attr(colcode, "table")))), "")
  plot(cen, border=NA, col=colcode)
  legend("left", # position
         legend = legcode, #names(attr(colcode, "table")), 
         fill = attr(colcode, "palette"), 
         cex = 1.4, 
         border=NA,
         bty = "n")
}  


mycols <- colorRampPalette(brewer.pal(8, "Blues"))(101)



##############


# Define server logic for slider examples
shinyServer(function(input, output) {
  
  #############################
  output$text1 <- renderText({ 
    
    paste( 
      input$year
    )
    
    })
  
  #############################
   output$plot1 <- renderPlot({
     

     par(mar=c(0,0,1,0))
     par(oma=c(0,0,0,0))
     par(mfrow=c(1,1))
     
     
     TractFunFixed(var = cen.df[,paste0(input$yvaryear, input$year)],
              color="YlOrRd",
              myMax = ceiling(max(cen.df[,paste0(input$yvaryear, 1995)], na.rm=TRUE)))
     title(main=input$yvaryear)
     
     

     

   } )





#############################
output$plot2 <- renderPlot({
  

  
  par(mar=c(4,4,1,1))
  par(oma=c(1,1,1,1))
  par(mfrow=c(1,2))
  
 #############
 hist(cen.df[,paste0(input$yvaryear, input$year)],
      #xlim=c(0,20),
      freq=FALSE,
      #ylim=c(0,0.5),
      main=paste0("Distribution of ", input$yvaryear, " by census tract"),
      breaks=20,
      col=adjustcolor("black", alpha.f=0.1),
      border=adjustcolor("black", alpha.f=0.3),
      xlim=c(0, ceiling(max(cen.df[,paste0(input$yvaryear, 1995)], na.rm=TRUE))),
      xlab=input$yvaryear)    
  
  ##########
  plot(ts$year, 
       ts[,input$yvaryear],
       #ylim=c(0,10),
       xlab="Year",
       ylab=input$yvaryear, type="n",
       main=paste0("Weighted ", input$yvaryear, " by census tract"))
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
         "beige")
  
  
  lines(ts$year, 
        ts[,input$yvaryear],
       col=adjustcolor("black", alpha.f=0.5), type="l", lwd=3)
  points(input$year,
         ts[which(ts$year == input$year), input$yvaryear],
         col=adjustcolor("darkred", alpha.f=0.3), cex=3,
         pch=16)

  


  
#   plot(cen.df[,paste0(input$xvar, input$year)],
#        cen.df[,paste0(input$yvar, input$year)], type="n",
#        ylab=input$yvar,
#        xlab=input$xvar)
# 
#   rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
#          "beige")
#   
#   points(cen.df[,paste0(input$xvar, input$year)],
#        cen.df[,paste0(input$yvar, input$year)],
#        pch=16,
#        col=adjustcolor(mycols[1+round(cen.df$PtPov, digits=0)], alpha.f=0.99),
#        cex=((cen.df$Total1)/1000)^(1/2))
#   
#   points(cen.df[,paste0(input$xvar, input$year)],
#          cen.df[,paste0(input$yvar, input$year)],
#          col=adjustcolor("black", alpha.f=0.3),
#          cex=((cen.df$Total1)/1000)^(1/2))
#   
#   legend(x="topright",
#          pch=16,
#          pt.cex=(c(1000,5000,20000)/1000)^(1/2),
#          legend=c(1000,5000,20000),
#          col=adjustcolor("black", alpha.f=0.2),
#          title="Tract population",
#          bty="n",
#          border=FALSE)
#   
#   legend(x="right",
#          pch=16,
#          pt.cex=2,
#          legend=c(10, 50, 90),
#          col=adjustcolor(mycols[c(10,50,90)], alpha.f=0.5),
#          title="Percent poverty",
#          bty="n",
#          border=FALSE)
#   legend(x="right",
#          pch=1,
#          pt.cex=2,
#          legend=c(10, 50, 90),
#          col=adjustcolor("black", alpha.f=0.5),
#          title="Percent poverty",
#          bty="n",
#          border=FALSE)
  
  

  
})


#############################
output$plota <- renderPlot({
  
  #      TractFunFixed(var=cen.df[,input$var]*100,
  #                    color="YlOrRd")
  
  par(mar=c(4,4,1,1))
  par(oma=c(1,1,1,1))
  par(mfrow=c(1,2))
  
  
  #----------
  TractFun(var = cen.df[,input$xvar],
           color="YlOrRd",
           breaks=input$breaks)
  #title(main=input$xvar)
  title(main= joeNames$Label[which(joeNames$var == input$xvar )])
  
  
  
  #--------
  TractFun(var = cen.df[,input$yvar],
           color="YlOrRd",
           breaks=input$breaks)
  #title(main=input$yvar)
  title(main= joeNames$Label[which(joeNames$var == input$yvar )])
  
  

 
  
  
})

#############################
output$plotb <- renderPlot({
  
  #      TractFunFixed(var=cen.df[,input$var]*100,
  #                    color="YlOrRd")
  
  par(mfrow=c(1,1))
  par(mar=c(4,4,1,1))
  par(oma=c(1,1,1,1))
  
  #--------
  
  plot(cen.df[,paste0(input$xvar)],
       cen.df[,paste0(input$yvar)], type="n",
       
       xlab=joeNames$Label[which(joeNames$var == input$xvar)],
       ylab=joeNames$Label[which(joeNames$var == input$yvar)])
       
       #xlab=input$xvar,
       #ylab=input$yvar)
  
  title(main=paste0("Correlation between ", 
                    joeNames$Label[which(joeNames$var == input$xvar)], 
                    " and ",
                    joeNames$Label[which(joeNames$var == input$yvar)],
                    " (shading: ",
                    joeNames$Label[which(joeNames$var == input$bub)],
                    ")"),
        cex.main=1.2)
  
#   rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
#          "beige")
  
  points(cen.df[,paste0(input$xvar)],
         cen.df[,paste0(input$yvar)],
         pch=16,
         col=adjustcolor(mycols[1+round(cen.df[,input$bub], digits=0)], alpha.f=0.8),
         cex=((cen.df$Total1)/1000)^(1/2))
  
  points(cen.df[,paste0(input$xvar)],
         cen.df[,paste0(input$yvar)],
         col=adjustcolor("black", alpha.f=0.3),
         cex=((cen.df$Total1)/1000)^(1/2))
  
  legend(x="topright",
         pch=16,
         pt.cex=(c(1000,5000,20000)/1000)^(1/2),
         legend=c(1000,5000,20000),
         col=adjustcolor("black", alpha.f=0.2),
         title="Tract population",
         bty="n",
         border=FALSE)
  
  legend(x="bottomright",
         pch=16,
         pt.cex=2,
         legend=c(10, 50, 90),
         col=adjustcolor(mycols[c(10,50,90)], alpha.f=0.8),
         title="Bubble shading (%)",
         bty="n",
         border=FALSE)
  legend(x="bottomright",
         pch=1,
         pt.cex=2,
         legend=c(10, 50, 90),
         col=adjustcolor("black", alpha.f=0.5),
         title="Bubble shading (%)",
         bty="n",
         border=FALSE)   
  
  
  

})


#############################
output$plotc <- renderPlot({
  
  #      TractFunFixed(var=cen.df[,input$var]*100,
  #                    color="YlOrRd")
  
  par(mar=c(4,4,1,1))
  par(oma=c(1,1,1,1))
  par(mfrow=c(1,2))
  
  hist(cen.df[,input$xvar],
       main=NA,
       col=adjustcolor("black", alpha.f=0.1),
       border=adjustcolor("black", alpha.f=0.3),
       xlab=cen.df[,input$yvar])
  #title(main=input$xvar)
  title(main= joeNames$Label[which(joeNames$var == input$xvar )])
  
  
  
  hist(cen.df[,input$yvar],
       main=NA,
       col=adjustcolor("black", alpha.f=0.1),
       border=adjustcolor("black", alpha.f=0.3),
       xlab=cen.df[,input$yvar])
  #title(main=input$yvar)
  title(main= joeNames$Label[which(joeNames$var == input$yvar )])
  

  
})



#############################
output$plot5 <- renderPlot({
  
  par(mar=c(1,2,1,1))
  par(oma=c(1,1,1,1))
  par(mfrow=c(1,2))
  
  #barplot(1:5)

   TractFun2(var = cen.df[,"perTested2010Age0004"], 
             col=ifelse(input$col == "TRUE", "Greens", "Greys"))
   title(main="Estimated percent of 0-4 year-olds tested in 2010")
  
  plot(ts$year, ts$nTests,
       xlab="Year", 
       ylab="Number of tests",
       type="l",
       col=ifelse(input$col == "TRUE", adjustcolor("darkred", alpha.f=0.6), adjustcolor("black", alpha.f=0.6)),
       lwd=2,
       ylim=c(0, max(ts$nTests, na.rm=T)))
  

})




#############################
output$plot6 <- renderPlot({
  
  par(mar=c(1,2,1,1))
  par(oma=c(1,1,1,1))
  
  barplot(1:5)
  
  
})


################################
output$table1 = renderTable({
  rownum = which(preds$Response==input$resp & preds$Model==input$model)
  outdf = matrix(as.integer(preds[rownum,3:6]), nrow=2, byrow=T)
  
  #rownames(outdf) = c("actual ABSENT", "actual PRESENT")
  rownames(outdf) <- c(paste0("Actual ", preds$Response.Name[which(preds$Response == input$resp)][1], " absent"), 
                       paste0("Actual ", preds$Response.Name[which(preds$Response == input$resp)][1], " present"))
  
  #colnames(outdf) = c("predicted ABSENT", "predicted PRESENT")
  colnames(outdf) <- c(paste0("Predicted ", preds$Response.Name[which(preds$Response == input$resp)][1], " absent"), 
                       paste0("Predicted ", preds$Response.Name[which(preds$Response == input$resp)][1], " present"))
  
  outdf
})

################################
output$splot1 = renderPlot(
  height=400,
  width=700,
{
  par(mfrow=c(1,1))
  rownum = which(preds$Response==input$resp & preds$Model==input$model)
  innums = as.integer(preds[rownum,c(3,4,6,5)])
  s0 = innums[1]+innums[2]
  s1 = innums[3]+innums[4]
  props = c(innums[1]/s0, innums[2]/s0,innums[3]/s1,innums[4]/s1)
  
  gr = rep(adjustcolor(c("darkgreen","red"), alpha.f=0.6), ,2)
  mybp = barplot(props*100, col=gr, width=0.2, space=c(.2,0,.2,0),
                 axes=F, main="Proportions predicted",
                 ylab="Proportion",
                 xlab="Actual status",
                 ylim=c(0,100))
  
  #joe
  text(x=mybp[,1],
       y=10,
       labels=paste0(round(props*100, digits=2), " %"),
       cex=1)
  
  #/joe
  
  axis(1, at=mybp[c(1.5,3.5),], labels=c("Absent","Present"), tick=F)
  axis(2)
  legend("topright",c("Correct","wrong"), fill=gr)
  #mybp
  

  
})

################################
output$splot2 = renderPlot(
    height=400,
    width=600,
  
  {

  plot(preds$falsepos[which(preds$Response == input$resp)], 
       1-preds$falseneg[which(preds$Response == input$resp)],
       xlab="False positive rate",
       ylab="True positive rate",
       pch=16,
       col=adjustcolor("black", alpha.f=0.6),
       cex=1.2,
       xlim=c(0,1),
       ylim=c(0,1),
       main=paste0("ROC curve for ", preds$Response.Name[which(preds$Response == input$resp)][1]))
  points(preds$falsepos[which(preds$Response == input$resp &
                                preds$Model == input$model)], 
         1-preds$falseneg[which(preds$Response == input$resp &
                                  preds$Model == input$model)],
         pch=17,
         col="red",
         cex=1.5)
  lines(0:1, 0:1, lty=6)
  
  })





################################
output$txt1 = renderText({
  rownum = which(preds$Response==input$resp & preds$Model==input$model)
  paste("Misclassification rate =", preds[rownum,7])})

################################
output$txt2 = renderText({
  rownum = which(preds$Response==input$resp & preds$Model==input$model)
  paste("False positive rate =", preds[rownum,8])})

################################
output$txt3 = renderText({
  rownum = which(preds$Response==input$resp & preds$Model==input$model)
  paste("False negative rate =", preds[rownum,9])})





})