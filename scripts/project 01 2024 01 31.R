### Step 0
#install.packages('tidyverse')
#install.packages('explore')
#install.packages('GGally')
library(tidyverse)
library(explore)
library(GGally)

### Step 2
dc.data2024 <- read.csv("https://opendata.arcgis.com/datasets/c5a9f33ffca546babbd91de1969e742d_6.csv", stringsAsFactors = FALSE)
dc.data2023 <- read.csv("https://opendata.arcgis.com/datasets/89561a4f02ba46cca3c42333425d1b87_5.csv", stringsAsFactors = FALSE)
dc.data2022 <- read.csv("https://opendata.arcgis.com/datasets/f9cc541fc8c04106a05a1a4f1e7e813c_4.csv", stringsAsFactors = FALSE)
dc.data2021 <- read.csv("https://opendata.arcgis.com/datasets/619c5bd17ca2411db0689bb0a211783c_3.csv", stringsAsFactors = FALSE)
dc.data2020 <- read.csv("https://opendata.arcgis.com/datasets/f516e0dd7b614b088ad781b0c4002331_2.csv", stringsAsFactors = FALSE)
dc.data2019 <- read.csv("https://opendata.arcgis.com/datasets/f08294e5286141c293e9202fcd3e8b57_1.csv", stringsAsFactors = FALSE)
dc.data2018 <- read.csv("https://opendata.arcgis.com/datasets/38ba41dd74354563bce28a359b59324e_0.csv", stringsAsFactors = FALSE)
dc.data2017 <- read.csv("https://opendata.arcgis.com/datasets/6af5cb8dc38e4bcbac8168b27ee104aa_38.csv", stringsAsFactors = FALSE)
dc.data2016 <- read.csv("https://opendata.arcgis.com/datasets/bda20763840448b58f8383bae800a843_26.csv", stringsAsFactors = FALSE)
dc.data2015 <- read.csv("https://opendata.arcgis.com/datasets/35034fcb3b36499c84c94c069ab1a966_27.csv", stringsAsFactors = FALSE)
dc.data2014 <- read.csv("https://opendata.arcgis.com/datasets/6eaf3e9713de44d3aa103622d51053b5_9.csv", stringsAsFactors = FALSE)
dc.data2013 <- read.csv("https://opendata.arcgis.com/datasets/5fa2e43557f7484d89aac9e1e76158c9_10.csv", stringsAsFactors = FALSE)
dc.data2012 <- read.csv("https://opendata.arcgis.com/datasets/010ac88c55b1409bb67c9270c8fc18b5_11.csv", stringsAsFactors = FALSE)
dc.data2011 <- read.csv("https://opendata.arcgis.com/datasets/9d5485ffae914c5f97047a7dd86e115b_35.csv", stringsAsFactors = FALSE)
dc.data2010 <- read.csv("https://opendata.arcgis.com/datasets/fdacfbdda7654e06a161352247d3a2f0_34.csv", stringsAsFactors = FALSE)

##### Step 3
data.temp <- rbind(dc.data2010, dc.data2011, 
                   dc.data2012, dc.data2013, dc.data2014, dc.data2015, 
                   dc.data2016, dc.data2017, dc.data2018, dc.data2019, 
                   dc.data2020, dc.data2021, dc.data2022, dc.data2023, 
                   dc.data2024)

##### Step 4
dc.data <- separate(data.temp, REPORT_DAT, into = c("DATE","TIME"), sep = " ")

##### Step 5
# format the date
dc.data$DATE <- as.Date(dc.data$DATE, format = "%Y/%m/%d")
# hour of day
dc.data$HOUR <- substr(dc.data$TIME,0,2)
dc.data$HOUR <- as.numeric(dc.data$HOUR)
# day of week
dc.data$DOW <- weekdays(dc.data$DATE)
# week of year
dc.data$WEEK <- format(as.Date(dc.data$DATE), "%U")
dc.data$WEEK <- as.numeric(dc.data$WEEK)
# month of year
dc.data$MONTH <- months(dc.data$DATE)
# year
dc.data$YEAR <- substr(dc.data$DATE,0,4)

##### Step 6
explore(dc.data)

##### Step 7
data.new <- select(dc.data, HOUR, DISTRICT, WARD, WEEK, YEAR)
ggpairs(data.new) + theme_bw()

ggpairs(data.new, cardinality_threshold = 53) + theme_bw()

##### Step 8
ggplot()
ggplot(dc.data, aes(SHIFT))
ggplot(dc.data, aes(SHIFT)) + geom_bar(stat = "count")
ggplot(dc.data, aes(SHIFT)) + geom_bar(stat = "count", aes(fill = OFFENSE))
ggplot(dc.data, aes(SHIFT)) + geom_bar(stat = "count", aes(fill = OFFENSE),
                                       position = position_stack(reverse = TRUE))
ggplot(dc.data, aes(SHIFT)) + geom_bar(stat = "count", aes(fill = OFFENSE),
                                       position = position_stack(reverse = TRUE)) +
  theme_classic()
ggplot(dc.data, aes(SHIFT)) + geom_bar(stat = "count", aes(fill = OFFENSE),
                                       position = position_stack(reverse = TRUE)) +
  theme_classic() + theme(legend.position = "none")
ggplot(dc.data, aes(SHIFT)) + geom_bar(stat = "count", aes(fill = OFFENSE), 
                                       position = position_stack(reverse = TRUE)) + 
  theme_classic() + theme(legend.position = "none") +
  facet_wrap(~YEAR)

# Formatting
graph <- ggplot(dc.data, aes(SHIFT)) + geom_bar(stat = "count")

graph +
  labs(
    title = "My title of this graph",
    x = "Shifts",
    y = "Crime Counts"
  )



