library(shiny)

setwd("E:/workingdirectory/shiny/")
source("census-app/helpers.R")

counties <- readRDS("census-app/data/counties.RDS")
percent_map(counties$white, "darkgreen", "% white")

runApp("census-app")


runApp("App-1")

#runApp("census-app", port = 8100)
