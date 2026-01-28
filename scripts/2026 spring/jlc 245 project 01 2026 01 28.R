# Step 0: Libraries
install.packages('tidyverse')
install.packages('explore')
install.packages('GGally')

library(tidyverse)
library(explore)
library(GGally)

# Step 1: examine data online

# Step 2
url <- "https://opendata.dc.gov/datasets/DCGIS::crime-incidents-in-"
years <- c(2008:2026)
full.urls <- paste0(url, years, ".csv")
dc.data <- data.frame()

for(file in full.urls) {
  tmp.data <- read.csv(file, stringsAsFactors = FALSE)
  dc.data <- rbind(tmp.data, dc.data)
}

# Step 3
dc.data <- separate(dc.data, REPORT_DAT, into = c("date", "time"), sep = " ")

# Step 4
dc.data$date <- as.Date(dc.data$date, format = "%Y/%m/%d")
dc.data$hour <- substr(dc.data$time, 0, 2)
dc.data$hour <- as.numeric(dc.data$hour)
dc.data$dow <- weekdays(dc.data$date)
dc.data$week <- format(dc.data$date, "%U")
dc.data$week <- as.numeric(dc.data$week) 
dc.data$months <- months(dc.data$date)
dc.data$month <- month(dc.data$date)
dc.data$year <- substr(dc.data$date, 0, 4)

# Step 5
explore(dc.data)






