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
    df$date <- as.Date(df$date, format = "%m/%d/%Y")
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
