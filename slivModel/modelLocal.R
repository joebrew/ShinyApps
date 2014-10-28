#TWO STEPS TO MODELLING
#1. CREATE THE GENERIC MATHEMATICAL/ALGEBRAIC MODEL 
#2. APPLY IT TO THE STATE (AND TO ALACHUA) AT DIFFERING LEVELS
##############################
##############################
##############################
##############################
##############################
##############################
##############################
##############################
##############################
##############################
##1. write the model (with personnel costs)
##############################

#COSTS ONLY
CostFun <- function(totMem, 
                    vfcPer,
                    immRate,
                    privateVacCost,
                    delivCost,
                    billCost,
                    printCost,
                    storageCost,
                    nursePerHour){
  
  kidsVaccinated <-totMem*immRate/100
  
  vfcVaccines <- kidsVaccinated*vfcPer/100
  
  privateVaccines <- totMem*(1-(vfcPer/100))*immRate/100
  
  otherCost <- delivCost + billCost + printCost + storageCost 
  
  privateCost <- privateVacCost *privateVaccines
  
  nurseCost <- (exp(3.26427 - (0.0038299*kidsVaccinated))) * 
    kidsVaccinated / 60 * nursePerHour
  
  privateCost + nurseCost + otherCost
}


# GROSS REVENUE ONLY
RevFun <- function(totMem,
                   vfcPer,
                   immRate,
                   privDenRate,
                   privUnbillableRate,
                   vfcDenRate,
                   privAvgRe,
                   vfcAvgRe){
  vfcVaccines <- totMem*vfcPer/100*immRate/100
  
  privateVaccines <- totMem*(1-(vfcPer/100))*immRate/100
  
  vfcRev <- vfcVaccines * vfcAvgRe * (1-(vfcDenRate/100))
  
  privateRev <- privateVaccines * privAvgRe * 
    (1-(privDenRate/100)) * (1-(privUnbillableRate/100))
  
  vfcRev + privateRev
}


#%%%%%%%%%%%%%%%%%% SANDBOX
CostFun(totMem = 1000, 
        vfcPer = 75,
        immRate = 70,
        privateVacCost = 17.5,
        delivCost = .05,
        billCost = 5.23,
        printCost = .1,
        storageCost = .05,
        nursePerHour = 25)



RevFun(totMem = 27826, 
       vfcPer = 41.787908,
       immRate = 46,
       privDenRate = 12.5,
       privUnbillableRate = 5.123104,
       vfcDenRate = 20,
       privAvgRe = 39.3,
       vfcAvgRe = 5)
#%%%%%%%%%%%%%%%% END SANDBOX


RPcost <- (immRate - 30)+(immrate-30)*0.02*totMem

2+2*.02

plot(RPcost, ylim=c(0,1), xlim=c(30,70))




##############################
##############################
##############################
##############################
##############################
##############################
##############################
##############################
#2. Apply to state
##############################
##############################



library(xtable)
library(maps)
library(RColorBrewer)
setwd("J:/Shared/Flumist data 2013/model")
##############################
### Make a color palette
##############################
blues <- colorRampPalette(brewer.pal(8, "Blues"))(140)
spectral <- colorRampPalette(brewer.pal(8, "Spectral"))(20)

##############################
### Read in school data
##############################
#setwd("J:/Shared/Flumist data 2013/model")
#totMem <- read.csv("totmem.csv", skip=0)

##############################
### Read in 2013/14 free lunch data
##############################
frLunch <- read.csv("frLunch13.csv")
#Make numeric columns into numeric R objects
frLunch$free <- as.numeric(gsub(",|#" ,"", frLunch$free))
frLunch$totMem <- as.numeric(gsub(",|#" ,"", frLunch$totMem))
frLunch$reduced <- as.numeric(gsub(",|#" ,"", frLunch$reduced))

##############################
### makes dfSchool and ADD APPROPRIATE COLUMNS
##############################
dfSchool <- frLunch[,c("district", "id", "School", "totMem", "free",
                       "reduced", "frper")]
dfSchool$frper <- as.numeric(as.character(dfSchool$frper))
#We want to use frPer as a PROXY indicator for the percentage of 
#students who are going to receive the VFC vaccine for each county
#frPer for Alachua = 49.026091
#vfcPer for Alachua = 41.78791  5773 / (5773+8042) * 100
# 41.78791 / 49.026091  = 0.8523606
#Therefore, dfCounty$vfcPer (our estimate) = 0.8523606*dfCounty$frPer
##############################
### Make vfcPer
##############################
dfSchool$vfcPer <- 0.8523606*dfSchool$frper

##############################
### COST OF EVERY SCHOOL AT 30% IMMUNIZATION RATE
##############################
dfSchool$imm30cost <- CostFun(totMem = dfSchool$totMem, 
                              vfcPer = dfSchool$vfcPer,
                              immRate = 30,
                              privateVacCost = 17.5,
                              delivCost = .05,
                              billCost = 5.23,
                              printCost = .1,
                              storageCost = .05,
                              nursePerHour = 25)

##############################
### GROSS REVENUE EVERY SCHOOL AT 30% IMMUNIZATION RATE
##############################
dfSchool$imm30rev <- RevFun(totMem = dfSchool$totMem, 
                            vfcPer = dfSchool$vfcPer,
                            immRate = 30,
                            privDenRate = 12.5,
                            privUnbillableRate = 5.123104,
                            vfcDenRate = 19.2,
                            privAvgRe = 39.3,
                            vfcAvgRe = 5)


#for (i in unique(sort(dfSchool$district))){
#  print(xtable(dfSchool[which(dfSchool$district == i),]))
#}

##############################
### NET REVENUE
##############################
dfSchool$imm30net <- dfSchool$imm30rev - dfSchool$imm30cost

#########
#Make a dataframe for counties
#######

dfCounty <- as.data.frame(unique(sort(dfSchool$district)))

colnames(dfCounty) <- "county"

####CReating a new column that has the sum of cost for each county
for(i in dfCounty$county){
  dfCounty$imm30cost[which(dfCounty$county == i)] <-
    sum(dfSchool$imm30cost[which(dfSchool$district == i)], na.rm=TRUE)
}

#####Create a new column in dfCounty that has the sum of revenues by county
for(i in dfCounty$county){
  dfCounty$imm30rev[which(dfCounty$county == i)] <-
    sum(dfSchool$imm30rev[which(dfSchool$district ==i)], na.rm=TRUE)
}


#####Create a new net column which is the difference between rev and cost for 
#each county

dfCounty$imm30net <- dfCounty$imm30rev - dfCounty$imm30cost

##########Create a column of totMem by county

for(i in dfCounty$county){
  dfCounty$totMem[which(dfCounty$county == i)] <-
    sum(dfSchool$totMem[which(dfSchool$district ==i)], na.rm=TRUE)
}

#######Create a column of revenue per kid by county
dfCounty$netPerKid <- dfCounty$imm30net / (dfCounty$totMem*.3)

#Remove the garbage counties
dfCounty <- dfCounty[which(dfCounty$county != "DEAF/BLIND" &
                             dfCounty$county != "FAMU LAB SCH" &
                             dfCounty$county != "FAU LAB SCH" &
                             dfCounty$county != "FL VIRTUAL" &
                             dfCounty$county != "FSU CHTR SCH" &
                             dfCounty$county != "UF LAB SCH" &
                             dfCounty$county != "WASH SPECIAL"),]

######Sandbox
dfCounty$county[which(dfCounty$imm30net < 1)]

sum(dfCounty$imm30net)

####End sandbox

library(maps)
myMap <- map("county", "florida")
myMap$county <- toupper(gsub("florida,|:main|:spit", "",myMap$names))
myMap$county <- gsub("ST", "ST.", myMap$county)
myMap$county <- gsub("DE SOTO", "DESOTO", myMap$county)
myMap$county <- gsub("GILCHRIST.", "GILCHRIST", myMap$county)

#MAP LOOP
for (i in unique(sort(myMap$county))){
  myMap$netPerKid[which(myMap$county == i)] <-
    dfCounty$netPerKid[which(dfCounty$county == i)]
}



myMap$color <- ifelse(myMap$netPerKid < 0,
                      "darkred",
                      ifelse(myMap$netPerKid >= 0 &
                               myMap$netPerKid < .6,
                             "darkorange",
                             ifelse(myMap$netPerKid >= .6 &
                                      myMap$netPerKid < 1.25,
                                    "yellow",
                                    ifelse(myMap$netPerKid >= 1.25 &
                                             myMap$netPerKid < 1.6,
                                           "darkgreen",
                                           ifelse(myMap$netPerKid >= 1.6 &
                                                    myMap$netPerKid < 2.5,
                                                  "darkblue",
                                                  ifelse(myMap$netPerKid >= 2.5,
                                                         "purple", 
                                                         "black"))))))

map("county", "florida", fill=TRUE, col=myMap$color)
save.image("J:/Shared/Flumist data 2013/model/model.RData")
