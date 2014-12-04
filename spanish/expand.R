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

subject <- dat$subject[1:10]
verb <- dat$verb
form <- dat$form[1:8]

z <- vector(length = 1, mode = "character")

for (i in 1:length(subject)){
  for (j in 1:length(verb)){
    for (k in 1:length(form)){
      x <- paste(subject[i],
            verb[j],
            form[k], sep = "+")
      z <- c(z, x)
    }
  }
}

# Split string on spaces
z <- data.frame(matrix(unlist(strsplit(z, split = "[+]")), ncol = 3, byrow = T))
names(z) <- c("subject", "verb", "form")

write.csv(z, "C:/Users/BrewJR/Desktop/z.csv")
