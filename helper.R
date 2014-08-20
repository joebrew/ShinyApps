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

library(RCurl)

# load("/home/joebrew/ShinyApps/cdph/ts.RData") # time series made from data
# ts$pTestsOver10 <- 100*ts$pTestsOver10
# ts$pTestsOver20 <- 100*ts$pTestsOver20
# save.image("/home/joebrew/ShinyApps/cdph/ts.RData")

# load("/home/joebrew/ShinyApps/cdph/july8.RData") #data
# x <- vector(length=ncol(cen.df), mode="character")
# for (i in 1:ncol(cen.df)){
#   x[i] <- class(cen.df[,i])
# }
# 
# for (i in colnames(cen.df)[x!= "factor"]){
#   
#   temp <- 
#     ifelse(max(cen.df[,x!="factor"][i], na.rm=T) >1,
#            cen.df[,x!="factor"][i],
#            cen.df[,x!="factor"][i]*100)
#   
#   cen.df[,x!="factor"][i] <- temp
# } 
# 
# rm(i, temp, x)
# save.image("/home/joebrew/ShinyApps/cdph/july8.RData")


myLink <- "https://docs.google.com/spreadsheets/d/1n0MKjb8LKzp1WB6nXZNp40d77Ey7UF4r-2FXGbAXA8U/export?&format=csv"

myCsv <- getURL(myLink)
cen.df <- read.csv(textConnection(myCsv))
rm(gpclibPermit, myCsv, myLink)
save.image("/home/joebrew/ShinyApps/cdph/cen.df.RData")
write.csv(cen.df, "/home/joebrew/ShinyApps/cdph/cen.df.csv")

# READ IN DATA FROM ANDREW'S SITE
# con <- url('http://www.people.fas.harvard.edu/~reece/joe/july7.RData') 
# load(con) 
# close(con) 
# 

#save.image("/home/joebrew/ShinyApps/cdph/july8.RData")

#SUBHOS DATA
# myLink <- "https://docs.google.com/spreadsheets/d/1WN-lX3YcclaeE6j3M8WX0jN5ySNA8tE0dak5ZRWzClk/export?&format=csv"
# myCsv <- getURL(myLink)
# preds <- read.csv(textConnection(myCsv))
# rm(myCsv, myLink)
# save.image("/home/joebrew/ShinyApps/cdph/preds.RData")

# 
# ts <- as.data.frame(1995:2013)
# colnames(ts) <- "year"
# 
# 
# # for (i in 1995:2013){
# #   ts$mean.bll[which(ts$year == i)] <-
# #     mean(cen.df[,paste0("mean.bll", i)], na.rm=TRUE)
# # }
# 
# for (i in 1995:2013){
#   ts$mean.bll[which(ts$year == i)] <-
#     weighted.mean(cen.df[,paste0("mean.bll", i)],
#                   cen.df[,paste0("nTests", i)], na.rm=TRUE)
# }
# 
# for (i in 1995:2013){
#   ts$median.bll[which(ts$year == i)] <-
#     median(cen.df[,paste0("mean.bll", i)], na.rm=TRUE)
# }
# 
# for (i in 1995:2013){
#   ts$nTests[which(ts$year == i)] <-
#     sum(cen.df[,paste0("nTests", i)], na.rm=TRUE)
# }
# 
# for (i in 1995:2013){
#   ts$pTestsOver10[which(ts$year == i)] <-
#     weighted.mean(cen.df[,paste0("pTestsOver10", i)],
#                   cen.df[,paste0("nTests", i)], na.rm=TRUE)
# }
# 
# for (i in 1995:2013){
#   ts$pTestsOver20[which(ts$year == i)] <-
#     weighted.mean(cen.df[,paste0("pTestsOver20", i)],
#                   cen.df[,paste0("nTests", i)], na.rm=TRUE)
# }
# 
# 
# for (i in 1995:2013){
#   ts$nTestsOver10[which(ts$year == i)] <-
#     weighted.mean(cen.df[,paste0("nTestsOver10", i)],
#                   cen.df[,paste0("nTests", i)], na.rm=TRUE)
# }
# 
# for (i in 1995:2013){
#   ts$nTestsOver20[which(ts$year == i)] <-
#     weighted.mean(cen.df[,paste0("nTestsOver20", i)],
#                   cen.df[,paste0("nTests", i)], na.rm=TRUE)
# }
# 
# rm(TractFun, TractFunFixed, TractFunFixed2, i, gpclibPermit, myCsv, myLink, cen.latlon, cen, cen.df)
# save.image("/home/joebrew/ShinyApps/cdph/ts.RData")
# 



"Percent non-latino white" = "PtNLWh",
"Percent non-latino black" = "PtNLB", 
"Percent latino" = "PtL"))
# CENSUS ADDS
load("/home/joebrew/ShinyApps/cdph/july8.RData") #data

myVars <- c("PtPov", "PtNLWh","PtNLB","PtL")
myYears <- 1995:2013
for (i in myVars){
  for(j in myYears){
    cen.df[,paste0(i,j)] <-
      cen.df[,i]
  }
}
rm(gpclibPermit, i, j, myCsv, myLink, myVars, myYears)
save.image("/home/joebrew/ShinyApps/cdph/july8.RData") #data












load("/home/joebrew/ShinyApps/cdph/july7.RData")
#source the budgetFun file

#################
# WRITE A PLOTTING FUNCTION FOR VISUALIZING
# MAPS OF TESTS BY CENSUS TRACT
#################
TractFun <- function(var, color){
  var[is.nan(var)] <- 0
  var[is.infinite(var)] <- 0
  plotvar <- var
  nclr <- 8
  plotclr <- brewer.pal(nclr, color)
  class <- classIntervals(plotvar, nclr, style = "quantile", dataPrecision=1) #use "equal" instead
  #class <- classIntervals(0:100, nclr, style="equal")
  colcode <- findColours(class, plotclr)
  legcode <- paste0(gsub(",", " - ", gsub("[[]|[]]|[)]", "", names(attr(colcode, "table")))), "")
  plot(cen, border=NA, col=colcode)
  legend("left", # position
         legend = legcode, #names(attr(colcode, "table")), 
         fill = attr(colcode, "palette"), 
         cex = 0.6, 
         border=NA,
         bty = "n")
}

TractFunFixed <- function(var, color ){
  var[is.nan(var)] <- 0
  var[is.infinite(var)] <- 0
  plotvar <- var
  nclr <- 9
  plotclr <- brewer.pal(nclr, color)
  class <- classIntervals(plotvar, nclr, style = "fixed", 
                          fixedBreaks=c(0,1,2,3,4,5, 10, 20, 100)) 
  #class <- classIntervals(0:100, nclr, style="equal")
  colcode <- findColours(class, plotclr)
  legcode <- paste0(gsub(",", " - ", gsub("[[]|[]]|[)]", "", names(attr(colcode, "table")))), "")
  plot(cen, border=NA, col=colcode)
  legend("left", # position
         legend = legcode, #names(attr(colcode, "table")), 
         fill = attr(colcode, "palette"), 
         cex = 0.6, 
         border=NA,
         bty = "n")
}


