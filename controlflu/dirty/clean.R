#setwd("C:/Users/BrewJR/Documents/ShinyApps/controlflu/dirty")


library(plyr)
ir <- read.csv("ir.csv")

# READ CFRR
cf <- read.csv("cfrr.csv")
cf$school <- as.character(gsub("^\\s+|\\s+$", "", cf$school_bad))

# READ CLASS CFRR AND IR DATA
cl <- read.csv("class.csv")
cl$school <- as.character(gsub("^\\s+|\\s+$", "", cl$School))

# FIX UP CL
library(car)
cl$school <- recode(cl$school,
                    "'Alachua ES' = 'Alachua';
                    'AQ Jones' = 'A Quinn';
                    'Archer Elem' = 'Archer';
                    'Finely' = 'Finley';
                    'Ft Clarke' = 'Ft. Clarke';
                    'GHS' = 'Gainesville';
                    'Glenn Springs' = 'Glen Springs';
                    'GlennSprings' = 'Glen Springs';
                    'HiddenOak' = 'Hidden Oak';
                    'High Springs' = 'High Spr. Comm.';
                    'High Springs Community' = 'High Spr. Comm.';
                    'HighSprings' = 'High Spr. Comm.';
                    'JJ Finley' = 'Finley';
                    'LakeForest' = 'Lake Forest';
                    'LittleWood' = 'Littlewood';
                    'Mabane' = 'Mebane';
                    'Metcalf' = 'Metcalfe';
                    'Oakview' = 'Oak View';
                    'OakView' = 'Oak View';
                    'SantaFe' = 'Santa Fe';
                    'WestWood' = 'Westwood'
                    ")

# GET ID FOR EVERY SCHOOL IN CL
cl <- join(x = cl, 
           y= cf,
           type = "left", 
           by = "school",
           match = "first")



# MERGE CFRR TO IR
#cf$school_bad <- NULL
ir <- join(x = ir,
           y = cf,
           by = "id",
           type = "left",
           match = "first")

# CONVERT ir$cfrr to percentage
ir$cfrr <- ir$cfrr*100

# CLEAN UP
cf$school_bad <- NULL

# ADD LOTS OF THINGS TO IR

#Create grade columns - cfrr
for (i in unique(sort(cl$Grade))){
  ir[,paste0("cfrr_grade",i)] <- NA
}

#POPULATE GRADE COLUMNS - cfrr
for (i in unique(sort(cl$Grade))){
  for (j in unique(ir$id)){
    for (k in unique(sort(ir$year))){
      
      ir[which(ir$year == k & ir$id == j), paste0("cfrr_grade", i)] <- 

      weighted.mean(cl$cfrr[which(cl$id == j & cl$year == k & cl$Grade == i)],
                    cl$Total.Pop[which(cl$id == j & cl$year == k & cl$Grade == i)])
   
    }
  }
}

#Create grade columns - immRate
for (i in unique(sort(cl$Grade))){
  ir[,paste0("immRate_grade",i)] <- NA
}

#POPULATE GRADE COLUMNS - immRate
for (i in unique(sort(cl$Grade))){
  for (j in unique(ir$id)){
    for (k in unique(sort(ir$year))){
      
      ir[which(ir$year == k & ir$id == j), paste0("immRate_grade", i)] <- 
        
        weighted.mean(cl$immRate[which(cl$id == j & cl$year == k & cl$Grade == i)],
                      cl$Total.Pop[which(cl$id == j & cl$year == k & cl$Grade == i)])
      
    }
  }
}

#CLEAN UP
ir$x <- NULL
ir$school_bad <- NULL
ir$team2014.1 <- NULL
ir$school.1 <- NULL


# WRITE CSVS FOR APP
#setwd("C:/Users/BrewJR/Documents/ShinyApps/controlflu/")
write.csv(cf, "cfrr.csv")
write.csv(ir, "ir.csv")

