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
#setwd("/home/joebrew/Documents/ShinyApps/roberts")

cf <- read.csv("cfrr.csv")
cl <- read.csv("cl.csv")
ir <- read.csv('ir.csv')
# library(RCurl)
# my_link <- "https://docs.google.com/spreadsheets/d/1icEDpqkJVNuvGLV6GcULuvfVK0healPyPep3enHkceE/export?gid=0&format=csv"
# options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
# my_csv <- getURL(my_link)
# ir <- read.csv(textConnection(my_csv))
# 
# ir$percent_free_reduced_lunch <- ir$frLunch13
# write.csv(ir, "ir.csv")
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