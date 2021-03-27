fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileURL, "./data.zip")
unzip("./data.zip")

library(readr)
DATA <- read_csv("activity.csv", na = "NA")