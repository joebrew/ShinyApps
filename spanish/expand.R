library(RCurl)
my_link <- "https://docs.google.com/spreadsheets/d/1voN1R9jqfVnIhlhOZdMpCrWHeFssk6J02uMkf8IT1oo/export?gid=0&format=csv"
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
my_csv <- getURL(my_link)
dat <- read.csv(textConnection(my_csv), stringsAsFactors = FALSE)

library(dplyr)
x <- dat %>%
  group_by(subject, verb, type, form) %>%
  summarise(n = n())

subject <- rep(dat$subject[1:10], each = 100)
verb <- rep(dat$verb, 100)
form <- rep(dat$form[1:8])
