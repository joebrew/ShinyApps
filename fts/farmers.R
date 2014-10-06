library(RCurl)
library(maps)
library(RColorBrewer)
##########
# SET WD
##########
# setwd("C:/Users/BrewJR/Documents/ShinyApps/fts")
# setwd("/home/joebrew/Documents/ShinyApps/fts")
############
# monthify function
############
Monthify <- function(x){
  format(x, format = "%B")
}


##########
# READ IN GOOGLE SPREADSHEET
##########
myLink <- "https://docs.google.com/spreadsheet/pub?key=0AsUfdoufBWMJdFV6a0p0NjdDRzJWQzA3WDFzRWxzQ3c&output=csv"
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
myCsv <- getURL(myLink)
farm <- read.csv(textConnection(myCsv), skip=0)
rm(myCsv, myLink)

# DEAL WITH TIMESTAMP ISSUES
timestamp <- as.character(farm$Timestamp)
timestamp <- sub(" .*", "", timestamp)
timestamp <- as.Date(timestamp, format="%m/%d/%Y")

###########
# READ IN THE NUMBER OF PREVIOUS GEOCODING
###########
number <- read.csv("number.csv")
number <- number$x

##########
# SUBSET TO ONLY INCLUDE COLUMNS FOR FINAL PRESENTATION
##########
farm <- farm[,c("Farm.name.", "Farmer.name.", "Farm.Mailing.Address.", "Zip.code.", 
                 "Business.Location.County.", "What.are.you.currently.growing.",
                 "Would.you.be.willing.to.expand.production.of.a.certain.crop.if.the.market.were.available.",
                 "DO.you.have.a.farm.food.safety.plan.",
                 "Have.you.ever.received.a.third.party.audit.",
                 "If.no..would.it.be.helpful.to.have.a.food.safety.training.in.this.county..",
                 "Do.you.currently.do.any.processing.or.packaging.",
                 "If.yes..what.type..",
                 "Do.you.have.an.inspected.audited.facility.for.processing.packing.",
                 "Would.you.be.willing.to.collaborate.with.a.co.op.to.improve.capacity.for.processing.or.packing.",
                 "Have.you.been.approached.in.the.past.to.provide.produce.to.schools.or.have.you.ever.tried.to.sell.to.schools.",
                 "What.is.your.farm.s.total.harvestable.acreage.",
                 "Do.you.deliver.your.product.to.customers.",
                 "If.no..how.do.you.handle.delivery.",
                 "What.are.your.current.sales.distribution.process.",
                 "What.counties.do.you.currently.serve.",
                 "NOTES")]

##########
# READ IN EXAMPLE OF FINAL PROJECT
##########
july <- read.csv("report_july.csv", skip=1)

###########
# ASSIGN NAMES OF JULY TO NAMES OF FARM
###########
names(farm) <- names(july)

##############
# ADD BACK IN TIMESTAMP
##############
farm$timestamp <- timestamp
farm$month <- Monthify(timestamp)

###########
# GEOCODE FARMS
###########
library(RCurl)
library(RJSONIO)
library(plyr)

url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

geoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type <- x$results[[1]]$geometry$location_type
    formatted_address <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
  } else {
    return(c(NA,NA,NA, NA))
  }
}

# Use plyr to getgeocoding for a vector
addressify <- function(x){
  address <- paste(as.character(gsub(" FL | fl | Fl |FL,| fl,| Fl,|,,", "", x)), "Florida")
  address[which(address == " Florida")] <- ""
  return(address)
}
farm$Farm.Mailing.Address. <- addressify(farm$Farm.Mailing.Address)

address <- farm$Farm.Mailing.Address.
address[which(address == "")] <- paste0(farm$Farm.name[which(address == "")], ", Florida")

###### GEOCODE NEW ROWS
if(nrow(farm) > number){
  
  #only geocode new rows
  locations <- ldply(address[if(nrow(farm) == (number+1)){(number+1)}else{(number+1):nrow(farm)}], function(x) geoCode(x))
  names(locations) <- c("lat","lon","location_type", "forAddress")
  
  
  ############
  # MAKE NA THE "FLORIDA" ONES
  ############
  locations$lat[which(locations$lat == 27.6648274 & locations$lon == -81.5157535)] <- NA
  locations$lon[which(is.na(locations$lat))] <- NA
  
  ############
  # CREATE GEOCODED DATAFRAME OF ADDRESSES AND POINTS
  ############
  new.gc <- data.frame(cbind(address[if(nrow(farm) == (number+1)){(number+1)}else{(number+1):nrow(farm)}], locations))
  names(new.gc)[1] <- "Farm.Mailing.Address."
  
  ############
  # READ IN OLD GC
  ############
  gc <- read.csv("gc.csv")
  gc$X <- NULL
  
  ############
  # RBIND OLD GC WITH NEW GC
  ############
  y <- as.data.frame(rbind(gc, new.gc))
  
  gc <- y
  rm(new.gc)

  #############
  # WRITE THE GC FOR ALL TIMES
  #############
  write.csv(gc, "gc.csv")
  
  ############
  # WRITE A CSV TO SAY HOW MANY ADDRESSES WERE ALREADY GEOCODED
  ############
  write.csv(nrow(gc), "number.csv")
  
}else{
  
  ############
  # READ IN OLD GC
  ############
  gc <- read.csv("gc.csv")
  gc$X <- NULL
  
}

############
# MERGE FARM AND GC
############
library(plyr)
farm <- join(x = farm,
          y = gc,
          by = "Farm.Mailing.Address.",
          type = "left",
          match = "first")

# REORDER COLUMN NAMES
farm2 <- farm[,c(names(july), "month")]


##############
# GET NEAREST SCHOOLS
###############
farm_geo <- read.csv("farm_geo.csv")

# RUN THE BELOW OCASIONALY, BUT TO SLOW TO RUN EACH TIME
# NOTE, DO THE SAME IN SERVER.R AT THE BOTTOM

# farm_geo <- farm
# library(fields)
# farm_geo$nearest5schools <- vector(length = nrow(farm_geo), mode = "character")
# for (i in 1:nrow(farm_geo)){
#   
#   x1 <- cbind(farm_geo$lon[i], farm_geo$lat[i])
#   x2 <- cbind(schools$LONGITUDE, schools$LATITUDE)
#   
#   x <- rdist.earth(x1, x2, miles = TRUE, R = NULL)
#   x <- data.frame(t(x))
#   names(x) <- "miles"
#   x$ind <- as.numeric(row.names(x))
#   x <- x[order(x$miles),]
#   
#   close_schools <- as.character(schools$SCHOOL_NAME_SHORT[x$ind[1:5]])
#   close_distances <- x$miles[1:5]
#   
#   mytext <- paste0(close_schools, " (", round(close_distances, digits = 1), " miles); ", collapse = "")
#   
#   farm_geo$nearest5schools[i] <- mytext
#   
# }
# write.csv(farm_geo, "farm_geo_geo.csv")

# 
# #############
# # ONE-OFF PLOT FOR KELLI (2014-09-29)
# #############
# par(mfrow=c(1,1))
# par(mar=c(0,0,0,0))
# par(oma=c(0,3,2,0))
# 
# #mycols <- adjustcolor(colorRampPalette(brewer.pal(8, "Dark2"))(nrow(farm)), alpha.f=0.8)
# mysize <- ifelse(farm$What.is.your.farm.s.total.harvestable.acreage. == "", 1,
#                  ifelse(farm$What.is.your.farm.s.total.harvestable.acreage. == "1-9", 2,
#                         ifelse(farm$What.is.your.farm.s.total.harvestable.acreage. == "1.5 Acres", 2,
#                                ifelse(farm$What.is.your.farm.s.total.harvestable.acreage. == "10-49", 3,
#                                       ifelse(farm$What.is.your.farm.s.total.harvestable.acreage. == "50-179", 4,
#                                              ifelse(farm$What.is.your.farm.s.total.harvestable.acreage. == "180-499", 5,
#                                                     ifelse(farm$What.is.your.farm.s.total.harvestable.acreage. == "500-999", 6,
#                                                            ifelse(farm$What.is.your.farm.s.total.harvestable.acreage.  == "1000+", 7,
#                                                                   1)))))))) 
# 
# mycols <- ifelse(farm$Have.you.ever.tried.to.sell.to.schools. == "", "black",
#                  ifelse(farm$Have.you.ever.tried.to.sell.to.schools. == "No", "darkred",
#                         ifelse(farm$Have.you.ever.tried.to.sell.to.schools. == "Yes", "darkgreen",
#                                "black")))
# mycols <- adjustcolor(mycols, alpha.f=0.3)
# 
# library(maps)
# map("county", "fl", fill=TRUE, col="grey", border="white")
# 
# points(farm$lon, farm$lat, 
#        col = adjustcolor(sample(rainbow(nrow(farm)) , nrow(farm)), alpha.f = 0.4),
#        #col= adjustcolor("darkblue", alpha.f=0.7), #mycols,
#        pch=16,cex = mysize/1.3)
# 
# # legend("bottomleft", pch=16, col=adjustcolor(c("black", "darkred", "darkgreen"), alpha.f=0.5),
# #        legend=c("Unknown", "No", "Yes"),
# #        title = "Ever tried to sell to schools?",
# #        bty="n", border=FALSE, cex=0.9)
# 
# legend("left", pch=1, col=adjustcolor("blue", alpha.f=0.5), pt.cex=(1:7)/1.3,
#        legend=c("Unknown", "1-9", "10-49", "50-179", "180-499", "500-999", "1000+"),
#        title = "Total harverstable acreage",
#        bty="n", border=FALSE, cex=0.9, y.intersp=1.6, ncol=2)
# 
# library(rgdal)
# zip <-  readOGR('/home/joebrew/Documents/uf/phc6194/hw4', layer = 'tl_2010_12_zcta510', verbose = FALSE)
# plot(zip)
# 
# farm_sp <- farm[which(!is.na(farm$lon) & !is.na(farm$lat)),]
# farm_sp$lat <- as.numeric(farm_sp$lat)
# farm_sp$lon <- as.numeric(farm_sp$lon)
# farm_sp <- SpatialPointsDataFrame(farm_sp[,c("lon", "lat")], farm_sp,
#                                              proj4string = CRS("+init=epsg:4326"))
# 
# 
# plot(zip, fill = TRUE, col = "white", border = "grey")
# 
# points(farm_sp, 
#        col = adjustcolor("darkred", alpha.f = 0.3), 
#        #col = "black",
#        #pch = 16,
#        pch=16,
#        cex = mysize/1.3)
