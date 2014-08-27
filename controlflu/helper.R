#######################
# ATTACH LIBRARIES
#######################

library(RCurl)
library(RColorBrewer)
suppressPackageStartupMessages(library(googleVis))
library(reshape)
library(reldist)
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
#setwd("C:/Users/BrewJR/Documents/ShinyApps/controlflu")
ir <- read.csv("ir.csv")
cf <- read.csv("cfrr.csv")


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

grades <- c(".1", 0:12)
gradecols <- colorRampPalette(brewer.pal(8, "Dark2"))(length(grades)) 
gradecols <- adjustcolor(gradecols, alpha.f=0.7)


par(mfrow=c(1,2))
plot(2006:2013, 2006:2013, 
     type="n", ylim=c(0,100), xlim=c(2011,2013),
     xlab= "Year", ylab= "Immunization rate",
     xaxt="n")
axis(side=1, at=2011:2013, labels=2011:2013)
for (i in 1:length(grades)){
  
  myVals <- as.numeric(as.character(ir[which(ir$school == "LITTLEWOOD ELEM."), paste0("immRate_grade", grades[i])]))
  myYears <- as.numeric(as.character(ir$year[which(ir$school == "LITTLEWOOD ELEM.")]))
  myPoints <- 12:25
  lines(myYears, myVals, type = "l", col=gradecols[i])
  points(myYears, myVals, col=gradecols[i],pch=myPoints)
  
  
  
}

legend(x="topright", col=gradecols, lty=1,
       legend=c("Pre-K", "K", grades[-c(1,2)]), ncol=2,
       border=FALSE, bty="n", pch=myPoints, cex=0.75)

plot(2006:2013, 2006:2013, 
     type="n", ylim=c(0,100), xlim=c(2011,2013),
     xlab= "Year", ylab= "Consent form return rate", xaxt="n")
axis(side=1, at=2011:2013, labels=2011:2013)

for (i in 1:length(grades)){
  
  myVals <- as.numeric(as.character(ir[which(ir$school == "LITTLEWOOD ELEM."), paste0("cfrr_grade", grades[i])]))
  myYears <- as.numeric(as.character(ir$year[which(ir$school == "LITTLEWOOD ELEM.")]))
  
  lines(myYears, myVals, type = "l", col=gradecols[i])
  points(myYears, myVals, col=gradecols[i], pch=myPoints)
  
}

par(mfrow=c(1,1))
tempVals <- vector(length=length(grades), mode="numeric")
tempImm <- vector(length=length(grades), mode="numeric")

for (i in 1:length(grades)){
  tempVals[i] <- as.numeric(as.character(ir[which(ir$school == "LITTLEWOOD ELEM." & 
                                               ir$year == 2013),
                                       paste0("cfrr_grade", grades[i])]))
  tempImm[i] <- as.numeric(as.character(ir[which(ir$school == "LITTLEWOOD ELEM." & 
                                                   ir$year == 2013),
                                           paste0("immRate_grade", grades[i])]))

}
barplot(tempVals[which(!is.na(tempVals))], names.arg=grades[which(!is.na(tempVals))])
barplot(tempImm[which(!is.na(tempImm))], col=adjustcolor("blue", alpha.f=0.6), add=TRUE)


# 
# 
# plot(x = 2006:2013, 
#      y = seq(0,100, length=length(2006:2013)),
#      type = "n",
#      xlab = "Year",
#      ylab = "Immunization rate",
#      ylim=c(0,100), main="All years")
# 
# x <- ir[which(ir$team2014 == 4),]
# 
# ids <- unique(sort(x$id))
# grades <- -1:12
# 
# for (i in ids){
#   for (j in grades){
#     
#     lines(x[which(x$id == ids[i]), paste0("year")],
#           x[which(x$id == ids[i]), paste0("cfrr_grade", j)])
#     
#     
#   }
# }
# 
# lines(x[which(x$id == ids[2]), paste0("year")],
#       x[which(x$id == ids[2]), paste0("cfrr_grade", 2)])
# 
# 
# GradeFun <- function(team=1, data=ir){
#   plot(x = 2006:2013, 
#        y = seq(0,100, length=length(2006:2013)),
#        type = "n",
#        xlab = "Year",
#        ylab = "Immunization rate",
#        ylim=c(0,100), main="All years")
#   
#   # PLOT LINES
#   
#   x <- data[which(data$team2014 == team),]
#   
#   for (i in -1:12){
#     
#     xx <- x$year[which(x[,paste0("immRate_grade", i)])]
#     yy <- x$immRate[, paste0("immRate_grade", i)]
#     
#   }
#   
#   for (i in 1:length(schools)){
#     
#     xx <- x$year[which(x$school == schools[i] )]
#     yy <- x$immRate[which(x$school == schools[i] )]
#     #x$color[which(x$school == schools[i])] <- mycols[i]
#     
#     cc <- x$color[which(x$school == schools[i])][1]
#     lines(xx, yy, col=cc, lwd=1)
#     points(xx, yy, col=cc, pch=16, cex=1)    
#   }  
#     
#   abline(v=2006:2013, col=adjustcolor("black", alpha.f=0.1))
#   abline(h=seq(0,100,20), col=adjustcolor("black", alpha.f=0.1))    
#   
# }
# 
# plot(ir$cfrr_grade1, ir$immRate_grade1)

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