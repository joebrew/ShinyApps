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

myLink <- "https://docs.google.com/spreadsheets/d/1rJrZwc9KmQsa2a7bOAX4wvPLc2p4lEtiuvxyufmE070/export?&format=csv"
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
myCsv <- getURL(myLink)

ir <- read.csv(textConnection(myCsv), skip=0)
rm(myCsv, myLink)

ir$team2014 <- factor(ir$team2014)

#######################
# PLOT
#######################
# M <- gvisMotionChart(data = ir, 
#                      idvar = "school", 
#                      timevar = "year",
#                      xvar = "year",
#                      yvar = "immRate",
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