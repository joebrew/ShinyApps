setwd("C:/Users/BrewJR/Documents/ShinyApps/controlfluteams")

#######################
# ATTACH LIBRARIES
#######################

library(RCurl)
library(RColorBrewer)
suppressPackageStartupMessages(library(googleVis))

#######################
# SOURCE HELPER
#######################
source("helper.R")
ir$school <- as.character(ir$school)
ir$school[which(ir$school == "HAWTHORNE JR./SR. HIGH")] <- "HAWTHORNE MIDDLE AND HIGH."

########################
# CONSTRUCT MODEL
########################
modelData <- ir[which(ir$type != "prek" & !is.na(ir$team2014)),]
testData <- modelData[which(modelData$year < 2013),]

fit <- lm(immRate ~ type + pubPriv + frLunch13, data = testData)

########################
# PREDICT USING MODEL
########################
modelData$predicted <- predict(fit, newdata = modelData)

########################
# PERFORMANCE SCORE
########################
modelData$performance <- modelData$immRate - modelData$predicted

x <- modelData[rev(order(modelData$performance)),]
x <- x[which(x$year == 2013),]

plot(x$predicted, x$immRate, pch=16)
abline(lm(x$immRate ~ x$predicted),
       col=adjustcolor("darkred", alpha.f=0.6))
