schools <- read.csv("schools.csv")

# RUN THE BELOW OCASIONALY, BUT TO SLOW TO RUN EACH TIME
# NOTE, DO THE SAME IN SERVER.R AT THE BOTTOM


# # Read in all schools
# schools <- read.delim("all_schools_complete.txt",
#                       sep = "\t",
#                       header = TRUE)
# schools <- schools[which(schools$LATITUDE != 0 & 
#                            schools$LONGITUDE != 0 &
#                            schools$LATITUDE < 32),]
#from: http://doeweb-prd.doe.state.fl.us/EDS/MasterSchoolID/Downloads.cfm?CFID=8116822&CFTOKEN=f38c3cf842534bc4-E679BC15-5056-8C3F-16E30D7156F7170B


# Use rdist.earth from the fields package to calculate distance between each school
# and each farm


# library(fields)
# schools$nearest5farms <- vector(length = nrow(schools), mode = "character")
# for (i in 1:nrow(schools)){
#   
#   x1 <- cbind(schools$LONGITUDE[i], schools$LATITUDE[i])
#   x2 <- cbind(farm$lon, farm$lat)
#   
#   x <- rdist.earth(x1, x2, miles = TRUE, R = NULL)
#   x <- data.frame(t(x))
#   names(x) <- "miles"
#   x$ind <- as.numeric(row.names(x))
#   x <- x[order(x$miles),]
#   
#   close_farms <- as.character(farm$Farm.name.[x$ind[1:5]])
#   close_distances <- x$miles[1:5]
#   
#   mytext <- paste0(close_farms, " (", round(close_distances, digits = 1), " miles); ", collapse = "")
#   
#   schools$nearest5farms[i] <- mytext
#     
# }
# write.csv(schools, "schools.csv")


