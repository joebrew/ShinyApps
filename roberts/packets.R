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


#######################
# WRITE CODE FOR PACKETS
#######################
PacketFun <- function(school, team= 0){
  
  if(team == 0){
    setwd("C:/Users/BrewJR/Documents/fdoh/public/controlflu2014")
    
  }else{
    setwd(paste0("C:/Users/BrewJR/Documents/fdoh/public/controlflu2014/team",team ))
    
  }
  
  pdf(file = paste0(school, ".pdf"), width = 11, height = 8.5)
  
  par(mar=c(5,5,5,5))
  par(oma=c(5,5,5,5))
  par(mfrow=c(1,1))
  plot(1:10, 1:10, type = "n", xaxt = "n", yaxt = "n", xlab=NA, ylab = NA)
  text(x=5.5, y = 8, labels = paste0(school), cex = 3,  pos = 3 )
  text(x=5.5, y = 6.5, labels = paste0("Control Flu"), cex = 2,  pos = 3 )
  
  text(x=5.5, y = 5.5, labels = "Data packet", cex = 2,  pos = 3 )
  text(x=5.5, y = 4.5, labels = paste0("Team ", ir$team2014[which(ir$school == school)][1]), cex = 2,  pos = 3 )
  
  mycols <- colorRampPalette(brewer.pal(8, "Dark2"))(length(seq(0,11,0.01))) 
  mycols <- adjustcolor(mycols, alpha.f=0.4)
  
  for (i in seq(30,70,1)){
    points(seq(0,11,0.01), seq(1,1.04,length=length(seq(0,11,0.01)))^i,
           pch=16,
           cex=0.2,
           #cex=seq(3,0, length= length(seq(0,11,0.1))),
           col=mycols)
  }
  par(mar=c(5,4,4,1))
  par(oma=c(1,1,3,1))

  par(mfrow=c(2,3))
  
  ScatterFun(school, 2011)
  title(main = "Consent form return and immunization rate (2011)", cex.main=0.9)
  ScatterFun(school, 2012)
  title(main = "Consent form return and immunization rate (2012)", cex.main=0.9)
  ScatterFun(school, 2013)
  title(main = "Consent form return and immunization rate (2013)", cex.main=0.9)
  

  GradeFun(school, 2011, bar=TRUE)
  GradeFun(school, 2012, bar=TRUE)
  GradeFun(school, 2013, bar=TRUE)
  

  #title(main= school, outer=TRUE, line = -0.5)
  
  par(mfrow=c(2,3))
  LunchFun(school, 2011)
  LunchFun(school, 2012)
  LunchFun(school, 2013)
  
  
  GradeFun(school, bar=FALSE)
  
  plot(1:10, 1:10, type = "n", xaxt = "n", yaxt = "n", xlab=NA, ylab = NA)
  text(x=5.5, y = 8, labels = paste0("Details"), cex = 2,  pos = 3 )
  
  text(x=5.5, y = 6, labels = paste0("Prepared by FDOH - Alachua County"), cex = 1,  pos = 3 )
  text(x=5.5, y = 5, labels = paste0("Joseph.Brew@flhealth.gov"), cex = 1,  pos = 3 )
  

  
  dev.off()
  
}

#######################
# WRITE ALL SCHOOLS
#######################

for (i in unique(sort(ir$school))){
  PacketFun(i)
}


#######################
# WRITE BY TEAM
#######################

# TEAM 1
for (i in unique(sort(ir$school[which(ir$team2014 == 1)]))){
  PacketFun(i, team = 1)
}


# TEAM 2
for (i in unique(sort(ir$school[which(ir$team2014 == 2)]))){
  PacketFun(i, team = 2)
}


# TEAM 3
for (i in unique(sort(ir$school[which(ir$team2014 == 3)]))){
  PacketFun(i, team = 3)
}


# TEAM 4
for (i in unique(sort(ir$school[which(ir$team2014 == 4)]))){
  PacketFun(i, team = 4)
}




#######################
#
#######################




#######################
#
#######################




#######################
#
#######################




#######################
#
#######################



