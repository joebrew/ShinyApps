#################
# SETWD CONDITIONAL TO SYSTEM I'M ON
#################

if ( Sys.info()["sysname"] == "Linux" ){
  setwd("/home/joebrew/Documents/ShinyApps/fts")
} else {
  setwd("C:/Users/BrewJR/Documents/ShinyApps/fts")
}

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
df <- data.frame("veg" = veg,
                 "dist" = dist,
                 "farm" = farm)

#######
# GIVE TRIMMED COST FOR FARM LETTUCE
#######
df$farmTrim <- df$farm
df$farmTrim[which(df$veg != "cuc")] <- 
  #(1.24)*df$farmTrim[which(df$veg != "cuc")] 
df$farmTrim[which(df$veg != "cuc")] / 0.76


plot(1:1000,seq(1,2500, length = 1000), type = "n",
     xlab = "Pounds purchased",
     ylab = "Price ($)")
for (i in seq(1,1000, length = 100)){
  
  # distributor lettuce
  points(i, df$dist[1]*i,
         col = adjustcolor("darkgreen", alpha.f =0.7),
         pch = "d", cex = 0.7)
  # distributor cucumbers
  points(i, df$dist[2]*i,
         col = adjustcolor("darkblue", alpha.f =0.7),
         pch = "d", cex = 0.7)
  # farmer lettuce
  points(i, (df$farmTrim[1]*i) + (rev(2*i))^(1/1.4) ,
         col = adjustcolor("darkgreen", alpha.f =0.7),
         pch = "f", cex = 0.7)
  # farmer cucumbers
  points(i, (df$farmTrim[2]*i) + (rev(2*i))^(1/1.2) ,
         col = adjustcolor("darkblue", alpha.f =0.7),
         pch = "f", cex = 0.7)
  
}
legend(x="topleft",
       pch = c("d","d", "f", "f"),
       col = adjustcolor(c("darkblue", "darkgreen", "darkblue", "darkgreen")),
       legend = c("Distributor cucumbers",
                  "Distributor lettuce",
                  "Farmer cucumbers",
                  "Farmer lettuce"))


points(1:1000,
       ((1:1000) * 0.6) + (rev(2*(1:1000))^(1/1.5)) )
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