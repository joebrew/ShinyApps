#######################
# ATTACH LIBRARIES
#######################

library(RCurl)
library(RColorBrewer)
suppressPackageStartupMessages(library(googleVis))

#######################
# READ IN IMMUNIZATION HISTORY FROM GOOGLE
#######################
# 
# myLink <- "https://docs.google.com/spreadsheets/d/1rJrZwc9KmQsa2a7bOAX4wvPLc2p4lEtiuvxyufmE070/export?&format=csv"
# options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
# myCsv <- getURL(myLink)
# 
# ir <- read.csv(textConnection(myCsv), skip=0)
# rm(myCsv, myLink)
# 
# ir$team2014 <- factor(ir$team2014)

# READ LOCALLY  - CHANGE LATER
#setwd("C:/Users/BrewJR/Documents/ShinyApps/controlfluteams")
ir <- read.csv("ir.csv")
cf <- read.csv("cfrr.csv")
cl <- read.csv("cl.csv")

#make team factor
ir$team2014 <- factor(ir$team2014)

#Make grade columns factors
for (i in 0:12){
  for (j in c("cfrr", "immRate")){
    ir[,paste0(j,"_grade",i)] <- 
      factor(ir[,paste0(j,"_grade",i)])
  }
}

# fix the -1s
ir$cfrr_grade.1 <- factor(ir$cfrr_grade.1)
ir$immRate_grade.1 <- factor(ir$immRate_grade.1)


#######################
# FUNCTION FOR PLOTTING BY GRADE
#######################
GradeFun <- function(school, year, bar=TRUE){

    grades <- c(".1", 0:12)
    gradecols <- colorRampPalette(brewer.pal(8, "Dark2"))(length(grades)) 
    gradecols <- adjustcolor(gradecols, alpha.f=0.7)
    
    
    if(!bar){
    #par(mfrow=c(1,2))
      par(mar=c(4,4,0,0))
    plot(2006:2013, 2006:2013, 
         type="n", ylim=c(0,100), xlim=c(2011,2013),
         xlab= NA, ylab= "Immunization rate",
         xaxt="n", cex.axis=1.5)
    
    text(x=2012, y = 15, labels=paste0("Immunization rate "),
         cex=1.5)
    text(x= 2012, y = 5, labels = paste0( "(", school, ")"))
    
    axis(side=1, at=2011:2013, labels=2011:2013)
    for (i in 1:length(grades)){
      
      myVals <- as.numeric(as.character(ir[which(ir$school == school), paste0("immRate_grade", grades[i])]))
      myYears <- as.numeric(as.character(ir$year[which(ir$school == school)]))
      myPoints <- 12:25
      lines(myYears, myVals, type = "l", col=gradecols[i])
      points(myYears, myVals, col=gradecols[i],pch=myPoints)
      
      
      
    }
    
    legend(x="topleft", col=gradecols, lty=1,
           legend=c("Pre-K", "K", grades[-c(1,2)]), ncol=2,
           border=FALSE, bty="n", pch=myPoints, cex=1)
    
    par(mar=c(4,4,0,0))
    
    plot(2006:2013, 2006:2013, 
         type="n", ylim=c(0,100), xlim=c(2011,2013),
         xlab= NA, ylab= "Consent form return rate", xaxt="n",
         cex.axis=1.5)
    text(x=2012, y = 15, labels=paste0("Consent form return rate "),
         cex=1.5)
    text(x= 2012, y = 5, labels = paste0( "(", school, ")"))
    
    axis(side=1, at=2011:2013, labels=2011:2013)
    
    for (i in 1:length(grades)){
      
      myVals <- as.numeric(as.character(ir[which(ir$school == school), paste0("cfrr_grade", grades[i])]))
      myYears <- as.numeric(as.character(ir$year[which(ir$school == school)]))
      
      lines(myYears, myVals,  lty = 1, col=gradecols[i])
      points(myYears, myVals, col=gradecols[i], pch=myPoints)
      
    }
    
  }else{
    
    if(length(ir$cfrr[which(ir$year == year & ir$school == school)]) == 0){
      return(NULL)
    }else{
    
    if(!is.na(ir$cfrr[which(ir$year == year & ir$school == school)])){
      #par(mfrow=c(1,1))
      tempVals <- vector(length=length(grades), mode="numeric")
      tempImm <- vector(length=length(grades), mode="numeric")
      
      for (i in 1:length(grades)){
        tempVals[i] <- as.numeric(as.character(ir[which(ir$school == school & 
                                                          ir$year == year),
                                                  paste0("cfrr_grade", grades[i])]))
        tempImm[i] <- as.numeric(as.character(ir[which(ir$school == school & 
                                                         ir$year == year),
                                                 paste0("immRate_grade", grades[i])]))
        
      }
      
      grades[which(grades == ".1")] <- "Pre-K"
      grades[which(grades == 0)] <- "K"
      
      
      bp <- barplot(tempVals[which(!is.na(tempVals))], 
                    names.arg=grades[which(!is.na(tempVals))],
                    border=NA, ylim=c(0,100), ylab="Percentage",
                    xlab="Grade",
                    main = paste0(school, " (", year, ")"))
      barplot(tempImm[which(!is.na(tempImm))], 
              col=adjustcolor("blue", alpha.f=0.4), add=TRUE,
              border=NA)
      text(bp[,1], tempVals[which(!is.na(tempVals))], pos=1,
           labels=round(tempVals[which(!is.na(tempVals))], digits=2),
           col = adjustcolor("black", alpha.f=0.7))
      text(bp[,1], tempImm[which(!is.na(tempImm))], pos=1,
           labels=round(tempImm[which(!is.na(tempImm))], digits=2),
           col = adjustcolor("black", alpha.f=0.7))
      legend(x="topleft",
             fill=adjustcolor(c("black", "blue"), alpha.f=0.7), 
             legend=c("CFRR", "IR"), bty= "n", border = FALSE, cex=0.75)
      
    }else{return(NULL)}
    

  }
  }
  
}

#GradeFun("B'NAI ISRAEL", 2011, bar=TRUE)

######################
# FREE REDUCED LUNCH FUN
######################
par(mar=c(5,4,2,1))
LunchFun <- function(school, year=2013){
  
  # THIS DOES ONLY 2013
  
  type <- ir$type[which(ir$school == school)][1]
  
  frl <- ir$frLunch13[which(ir$school != school & ir$year == year
                            & ir$type == type)]
  
  cfrr <-  ir$cfrr[which(ir$school != school & ir$year == year &
                           ir$type == type)]
  
  
  
  frl_school <- ir$frLunch13[which(ir$school == school & ir$year == year)]
  cfrr_school <-  ir$cfrr[which(ir$school == school & ir$year == year)]
  
  
  schoolcol <- adjustcolor("darkred", alpha.f=0.6)
  othercol <- adjustcolor("black", alpha.f=0.3)
  
  plot(0:100, 0:100, 
       xlab = "Percent free/reduced lunch",
       ylab = "Consent form return rate", 
       main = paste0("Free/reduced lunch and CFRR (", year, ")"),
       xlim=c(0,100),
       ylim=c(0,100), type="n")
  points(frl, cfrr, col=othercol, pch=16, cex = 2)
  points(frl_school, cfrr_school, pch=16, col=schoolcol, cex=3)
  
  legend(x="bottomleft",
         pt.cex=c(2,3),
         pch=16,
         col=c(othercol, schoolcol),
         legend=c(paste0(school, " (", year, ")"),
                  paste0("Other ", type, " schools (", year, ")")), 
         cex=0.8)
}

#LunchFun("LITTLEWOOD ELEM.", 2013)


#######################
# CFRR AND IR BY CLASSROOM
#######################
ScatterFun <- function(school, year=2013){
  

  
  type <- as.character(ir$type[which(ir$school == school)][1])
  
  newcl <- cl[which(as.character(cl$type) == type & cl$year == year),]
  schoolcl <- newcl[which(newcl$id == ir$id[which(ir$school == school )][1]),]
  
  schoolcol <- adjustcolor("darkred", alpha.f=0.6)
  othercol <- adjustcolor("black", alpha.f=0.3)
  
  meancfrr <- weighted.mean(newcl$cfrr, newcl$Total.Pop, na.rm=T)
  meanir <- weighted.mean(newcl$immRate, newcl$Total.Pop, na.rm=T)
  
  schoolmeancfrr <- weighted.mean(schoolcl$cfrr, schoolcl$Total.Pop, na.rm=T)
  schoolmeanir <- weighted.mean(schoolcl$immRate, schoolcl$Total.Pop, na.rm=T)
  
  
  plot(0:100, 0:100, 
       xlab = "CFRR",
       ylab = "IR", 
       xlim=c(0,100),
       ylim=c(0,100), type="n")
  
  points(newcl$cfrr, newcl$immRate, pch=16, 
       col=adjustcolor(othercol, alpha.f=0.2),
       cex=(newcl$Total.Pop)^(1)/10)
  points(schoolcl$cfrr, schoolcl$immRate, 
         col=schoolcol,
         cex=schoolcl$Total.Pop^(1)/10,
         pch=16)
  
  legend(x="bottomleft",
         col=c(othercol, schoolcol),
         pch=16,
         legend=c(paste0("Other ", type, " schools"), school),
         border=FALSE,  cex=0.7)
  
  legend(x="topleft",
         col=c(othercol, schoolcol),
         lty=1, lwd=3,
         legend=c(paste0("Other ", type, " schools weighted mean"), paste0(school, " mean")),
         border=FALSE, bty= "n", cex=0.7)
  
  
  abline(h=meanir, col=othercol, lwd=3)
  abline(v=meancfrr, col=othercol, lwd=3)
  
  abline(h=schoolmeanir, col=adjustcolor(schoolcol, alpha.f=0.6), lwd=3)
  abline(v=schoolmeancfrr, col=adjustcolor(schoolcol, alpha.f=0.6), lwd=3)
  
}

#ScatterFun("LITTLEWOOD ELEM.", 2013)

#######################
# FUNCTION FOR PLOTTING EACH TEAM'S TRAJECTORIES
#######################
TeamFun <- function(team, data, cf=FALSE, bar=FALSE){
  
  x <- data[which(as.numeric(data[,"team2014"]) == as.numeric(team)),]
  
  schools <- unique(x$school)   #unique(sort(x$school))
  mycols <- colorRampPalette(brewer.pal(8, "Dark2"))(length(schools)) 
  mycols <- adjustcolor(mycols, alpha.f=0.4)
  
  # ASSIGN COLOR
  x$color <- "grey"
  for (i in 1:length(schools)){    
    x$color[which(x$school == schools[i])] <- mycols[i]
  }  
  
  
  if(bar==FALSE & cf==FALSE){
    plot(x = 2006:2013, 
         y = seq(0,100, length=length(2006:2013)),
         type = "n",
         xlab = "Year",
         ylab = "Immunization rate",
         ylim=c(0,100), main="All years")
    
    # PLOT LINES
    for (i in 1:length(schools)){

      xx <- x$year[which(x$school == schools[i] )]
      yy <- x$immRate[which(x$school == schools[i] )]
      #x$color[which(x$school == schools[i])] <- mycols[i]
      
      cc <- x$color[which(x$school == schools[i])][1]
      lines(xx, yy, col=cc, lwd=1)
      points(xx, yy, col=cc, pch=16, cex=1)    
    }  
    
    legend(x="topleft",
           lty=1,
           lwd=1,
           pch=16,
           col=mycols,
           legend=schools,
           cex=0.7,
           bty="n",
           ncol=3)
    
    abline(v=2006:2013, col=adjustcolor("black", alpha.f=0.1))
    abline(h=seq(0,100,20), col=adjustcolor("black", alpha.f=0.1))  
  }else if(cf==TRUE & bar==FALSE){
    
    x <- x[which(x$year == 2013),]
    
    plot(x$cfrr, x$immRate, col=x$color, pch=16,
         xlim=c(0,100), ylim=c(0,100),
         cex=3,
         xlab="2013 consent form return rate",
         ylab="2013 immunization rate")
    
    text(jitter(x$cfrr, factor=5), jitter(x$immRate, factor=5), col=adjustcolor("black", alpha.f=0.6),
         labels=x$school,
         cex=0.6)
    abline(v=seq(0,100,20), col=adjustcolor("black", alpha.f=0.1))
    abline(h=seq(0,100,20), col=adjustcolor("black", alpha.f=0.1))  
         
    
  }else if(cf==TRUE & bar == TRUE){ 
    x <- x[which(x$year == 2013),]
    x <- x[order(x$cfrr),]
    
    bp <- barplot(x$cfrr, col=x$color, names.arg=x$school, cex.names=0.7,
                  border=FALSE, las=3, main="2013 Consent form return rate",
                  ylab="Consent form return rate")
    
    text(x=bp[,1], y = x$cfrr, pos=1, labels=round(x$cfrr, digits=2),
         col=adjustcolor("black", alpha.f=0.5))
  }else{
    # BARPLOT
    
    x <- x[which(x$year == 2013),]
    x <- x[order(x$immRate),]
    
    bp <- barplot(x$immRate, col=x$color, names.arg=x$school, cex.names=0.7,
            border=FALSE, las=3, main="Last year",
            ylab="Immunization rate")
    
    text(x=bp[,1], y = x$immRate, pos=1, labels=round(x$immRate, digits=2),
         col=adjustcolor("black", alpha.f=0.5))

  } 

}

#TeamFun(team = 4, data = ir, bar = FALSE)
#######################
# PLOT
# #######################
# M <- gvisMotionChart(data = ir, 
#                      idvar = "school",
#                      timevar = "year")
# , 
#                      idvar = "school", 
#                      timevar = "year",
#                      #xvar = "year",
#                      #yvar = "immRate",
#                      colorvar = "team2014",
#                      sizevar = "totMem")
# plot(M)
# 
# M <- gvisMotionChart(data = ir, idvar="school", timevar="year")
# 
# 
# plot(M)

#######################
#
#######################


#######################
#
#######################