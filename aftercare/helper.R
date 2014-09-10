
########
# READ IN PARENTS RESPONSES FROM GOOGLE DRIVE
########
require(RCurl)
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
myLink<- "https://docs.google.com/spreadsheets/d/1DhRFBd3pcHjQNhnT6qW10P0TCx-Pb3JVLMZVFoYXCYU/export?&format=csv"
myCsv <- getURL(myLink)
df <- read.csv(textConnection(myCsv))

########
# CHANGE COLUMN NAMES
########
names(df) <- c("time", "name", "grade", "mon", "tue" ,"wed", "thu", "fri", "comments")

########
# MAKE FUNCTION TO RENAME LEVELS TO MINUTES AFTER 3
########
MinuteFun <- function(var){
  ifelse(df[,var] == "", 0,
         ifelse(df[,var] == "Until 3:30 (1-3 graders only)", 30,
                ifelse(df[,var] == "Until 4", 60,
                       ifelse(df[,var] == "Until 4:30", 90,
                              ifelse(df[,var] == "Until 5", 120, 
                                     ifelse(df[,var] == "Not interested", 0, 
                                            NA))))))
}

# NUMERICAL MINUTES AFTER 3:00
df$mon1 <- MinuteFun("mon")
df$tue1 <- MinuteFun("tue")
df$wed1 <- MinuteFun("wed")
df$thu1 <- MinuteFun("thu")
df$fri1 <- MinuteFun("fri")


# CHARGED MINUTES
# this conditional takes away 30 minutes from 4-6 graders if they choose after care
# mon
df$mon1c <- ifelse(df$grade <=3, df$mon1,
                   ifelse(df$mon1 == 0, df$mon1,
                          ifelse(df$grade >=4, df$mon1 - 30, NA)))
# tue
df$tue1c <- ifelse(df$grade <=3, df$tue1,
                   ifelse(df$tue1 == 0, df$tue1,
                          ifelse(df$grade >=4, df$tue1 - 30, NA)))
# wed
df$wed1c <- ifelse(df$grade <=3, df$wed1,
                   ifelse(df$wed1 == 0, df$wed1,
                          ifelse(df$grade >=4, df$wed1 - 30, NA)))
# thu
df$thu1c <- ifelse(df$grade <=3, df$thu1,
                   ifelse(df$thu1 == 0, df$thu1,
                          ifelse(df$grade >=4, df$thu1 - 30, NA)))
# fri
df$fri1c <- ifelse(df$grade <=3, df$fri1,
                   ifelse(df$fri1 == 0, df$fri1,
                          ifelse(df$grade >=4, df$fri1 - 30, NA)))


days <- c("mon", "tue", "wed", "thu", "fri")
df <- df[,c("grade", paste0(days,1), paste0(days,"1c"))]

young <- df[which(df$grade <=3),]
old <- df[which(df$grade >=4),]
#########
# FUNCTION FOR MINUTES PER DAY
#########
Fun <- function(day, minutes){
  
  sum(young[,paste0(day,"1c")][which(young[,paste0(day,1)] <= minutes)]) +
    sum(old[,paste0(day,"1c")][which(old[,paste0(day,1)] <= minutes)]) 
}

########
# MAKE TS
########
day <- c("mon", "tue", "wed", "thu", "fri")
ts <- data.frame(day)

#########
# CREATE COLUMNS FOR TIME OPTIONS
#########
for (i in c(30,60,90,120)){
  ts[,paste0("x",i)] <- NA
}

########
# LOOP TO GET CHARGED MINUTES FOR EACH DAY / TIME
########
for (i in ts$day){
  for (j in c(30,60,90,120)){
    ts[which(ts$day == i), paste0("x", j)] <-
      Fun(i, j)
  }
}


########
# LOOP TO GET ROI - $10 / HOUR FOR TEACHER AND $10 / HOUR FOR KID
########
Hourfy <- function(x){x/60}

for (i in c(30,60,90,120)){
  ts[,paste0("roi",i)] <- NA
}


########
#
########


########
#
########


########
#
########


########
#
########


########
#
########


########
#
########


########
#
########


########
#
########


########
#
########


########
#
########


########
#
########



