library(RCurl)
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
#####
# DEFINE PUBLIC LINKS FOR AFTERCARE DATA
#####
stay_link <- "https://docs.google.com/spreadsheets/d/1aepPhs07f96SRk-hNj-Cmv5yedTcx66fZWStGUncm80/pubhtml"
pay_link <- "https://docs.google.com/spreadsheets/d/1o_CBGo728p4QIcBYm8lCLm2LPSjodElkZ5NyUJiIs4o/pubhtml"
roster_link <- "https://docs.google.com/spreadsheets/d/1xjNKkqKghhI_kY76CSph9W_jt-wOnSQWQBxbpzbZ2qg/pubhtml"

#####
# DEFINE FUNCTION TO READ DATA FROM https://docs.google.com/spreadsheets/d/1X6Zj3GqwjVAjiYYk_eMoeW4txWi9pmEHd0mqhbk1br4/pubhtml?gid=187478485&single=true
#####
get_google <- function(myLink){
  csvLink <- gsub("/pubhtml", "/export?&format=csv", myLink)
  myCsv <- getURL(csvLink)
  df <- read.csv(textConnection(myCsv), stringsAsFactors = FALSE)
  if("date" %in% names(df)){
    #df$date <- as.Date(df$date, format = "%m/%d/%Y")
  }
  df$name <- toupper(df$name)
  return(df)
}


#####
# READ IN DATA
#####
stay <- get_google(stay_link)
pay <- get_google(pay_link)
roster <- get_google(roster_link)
# stay <- read.csv("jan30/stay.csv")
# pay <- read.csv("jan30/pay.csv")
# roster <- read.csv("jan30/roster.csv")


#####
# Clean up
#####

# CLEAN UP SIBLING
stay$sibling <- stay$sibling == "yes"

# Fix dates
pay$date <- as.Date(pay$date, format = "%m/%d/%Y")
stay$date <- as.Date(stay$date, format = "%m/%d/%Y")

# Standardize names
roster$name <- toupper(roster$name)
pay$name <- toupper(pay$name)
stay$name <- toupper(stay$name)

# Clean up time_minute
stay$time_minute[which(is.na(stay$time_minute))] <- 0

# Ensure numeric
stay$time_hour <- as.numeric(stay$time_hour)
stay$time_minute <- as.numeric(stay$time_minute)

# Get month
stay$month <- as.numeric(format(stay$date, "%m"))

# Clean up roster
library(car)
roster$name <- Recode(roster$name,
                      "
                      'NATHAN' = 'NATHAN GOBLE';
                      'NAOMI' = 'NAOMI GOBLE';
                      'ZOLTAHN' = 'ZOLTHAN';
                      'SAWYER ' = 'SAWYER'
                      ")

# Merge roster to stay
library(dplyr)
stay <- left_join(x = stay,
                  y = roster,
                  by = "name") 

# Fix grades (if they're not there!!! THIS NEEDS TO BE FIXED)
# Sawyer, Casey, Max, Phethra
stay$grade <- as.numeric(stay$grade)

stay$grade[which(is.na(stay$grade))] <- 5
stay$grade <- as.numeric(stay$grade)

# Strategy: deal with stay first, pay later

#####
source("helpers.R")

df <- stay[-c(1:nrow(stay)),]
for (j in unique(sort(stay$name))){
  # Define name
  student_name <- j
  
  # Subset pay to just the days pertaining to that student
  student_stay <- stay[which(stay$name == student_name),]
  
  # Get price
  student_stay$price <- NA
  
  # Split September, other, and january and beyond
  sep <- student_stay[which(student_stay$month == 9),]
  nosep <- student_stay[which(student_stay$month > 9 &
                                student_stay$month <=12),]
  newyear <- student_stay[which(student_stay$month <=8),]
  
  # Deal first with just September
  if(nrow(sep) > 0){
    sep$price <- 
      get_september_price(df = sep)
  } 
  
  # Now deal with other months
  if(nrow(nosep) > 0){
    nosep$price <- 
      get_other_price(df = nosep)
  }
  
  # Now deal with new year
  # Now deal with other months
  if(nrow(newyear) > 0){
    newyear$price <- 
      get_new_price(df = newyear)
  }
  
  # Combine all three pay periods together
  new_df <- rbind(sep, nosep, newyear)
  
  # Merge into df
  df <- rbind(df, new_df)
}

#### GET PAYMENTS
paid <- function(name){
  sum(pay$amount[which(pay$name == name)], na.rm = TRUE)
}

payment_df <- function(name){
  temp <- pay[which(pay$name == name),]
  return(temp)
}

