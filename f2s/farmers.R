library(RCurl)

##########
# SET WD
##########
setwd("C:/Users/BrewJR/Documents/farm2school")

##########
# READ IN GOOGLE SPREADSHEET
##########
myLink <- "https://docs.google.com/spreadsheet/pub?key=0AsUfdoufBWMJdFV6a0p0NjdDRzJWQzA3WDFzRWxzQ3c&output=csv"
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
myCsv <- getURL(myLink)
farm <- read.csv(textConnection(myCsv), skip=0)
rm(myCsv, myLink)

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



