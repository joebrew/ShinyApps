setwd("C:/Users/BrewJR/Documents/ShinyApps/fts")

#######
# READ IN COSTS SPREADSHEET (SUPPLIED BY KELLI IN EMAIL)
#######
costs <- read.csv("costs.csv")

#######
# GIVE POUNDS COSTS TO CUCUMBERS
#######
costs$cukes_lbs <- costs$cukes_cts /3

#######
# CORRECT LETTUCE WITH TRIM
#######

costs$lettuceWithTrim <- costs$lettuce * (1/.76)

# 25 pounds of lettuce goes into one box
# 40 pounds of cucumbers per box
# DISTRIBUTORS DO 2-3$ PER BOX

# EVEN THOUGH BOX COST ISNT REAL FOR FAMRERS, GIVE PER BOX COST

# HOW MUCH DOES FARMERS LETTUCE COST (PER POUND) COMPARED TO DISTRIBUTOR LETTUCE

# GET EVERYTHING INTO PER POUND COSTS

#######
# NUMBERS FROM KELLI
########
veg <- c("lettuce", "cuc")
dist <- c(1.74,  2.38)
farm <- c(1.06, 0.60)

#######
# BIND INTO DATAFRAME
#######
df <- data.frame(veg)
df$dist <- dist
df$farm <- farm

#######
# GIVE TRIMMED COST FOR FARM LETTUCE
#######
df$farmTrim <- df$farm
df$farmTrim[which(df$veg != "cuc")] <- 
  #(1.24)*df$farmTrim[which(df$veg != "cuc")] 
df$farmTrim[which(df$veg != "cuc")] / 0.76


#######
#
#######



#######
#
#######



#######
#
#######



#######
#
#######