require(RCurl)
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))


myLink<- "https://docs.google.com/spreadsheets/d/1ckfJLebzPYhJCp595aUn7IXkbDBBB21kZfGREw0l5PE/export?&format=csv"

myCsv <- getURL(myLink)
df <- read.csv(textConnection(myCsv))


df$date <- as.Date(df$Date, "%m/%d/%Y")

#total minutes
df$tot <- vector(mode="numeric",
                 length=nrow(df))
for (i in 1:nrow(df)){
  df$tot[i] <- sum(df[1:i, "Minutes"], na.rm=T)
}

#total goal
df$tot.goal <- vector(mode="numeric",
                 length=nrow(df))
for (i in 1:nrow(df)){
  df$tot.goal[i] <- sum(df[1:i, "Goal"], na.rm=T)
}

df$tot[which(df$date > Sys.Date())] <- NA