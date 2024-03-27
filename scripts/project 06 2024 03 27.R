# step 0: libraries
install.packages('usmap')
install.packages('rvest')
install.packages('Hmisc')
library(tidyverse)
library(usmap)
library(rvest)
library(Hmisc)

# step 1: get data
# crate objects
years <- as.character(2014:2024)
pages <- 0:30
url <- 'https://www.gunviolencearchive.org/reports/mass-shooting'
urls <- paste0(url, "?page=", pages)

# build URLs
urls.all <- NA
for (file in urls) {
  tmp.url <- paste0(file, "&year=", years)
  urls.all <- c(tmp.url, urls.all)
}
urls.all <- urls.all[!is.na(urls.all)]

# create a function for scraping a table from each URL
get.gva <- function(site) {
  site %>%
    read_html()%>%
    html_nodes(xpath = '//*[@id="content"]/div/div/div') %>%
    html_table()
}

# go get the data
results <- sapply(urls.all, get.gva)

# transform the data into a table
data.gva <- data.frame()
for (urls in results) {
  tmp.gva <- data.frame(urls)
  data.gva <- rbind(tmp.gva, data.gva)
}

# step 2: cleaning data
data.gva$date <- as.Date(data.gva$Incident.Date, format = "%B %d, %Y")
data.gva$source <- "Gun Violence Archive"
data.gva$total.victims <- data.gva$Victims.Killed + data.gva$Victims.Injured
data.gva$day <- weekdays(data.gva$date)
data.gva$year <- substr(data.gva$date, 0, 4)
data.gva$month <- substr(data.gva$date, 6, 7)
data.gva$yearmonth <- paste(data.gva$year, data.gva$month, sep = "-")
data.gva <- data.gva[c(1,12,4,3,7,6,14,15,17,16,18,13)]
names(data.gva) <- c("event", "date", "city", "state", "killed", 
                     "injured", "total", "day", "month", "year", 
                     "yearmonth", "source")
data.gva <- data.gva[!duplicated(data.gva), ]

data.gva$citystate <- paste(data.gva$state, data.gva$city, sep = "-")

# step 3: summarize
events.state <- data.gva %>% 
  group_by(state) %>% 
  summarise(count = n()) %>%
  mutate(pct = count/sum(count)*100)

events.city <- data.gva %>%
  group_by(state, city) %>%
  summarise(count = n())
events.city$diff <- abs(events.city$count - events.city$count[256])

events.year <- data.gva %>% 
  group_by(year) %>% 
  summarise(count = n()) %>%
  mutate(pct = count/sum(count)*100)

events.year.city.state <- data.gva %>% 
  group_by(year, state, city) %>% 
  summarise(count = n())

victims.state <- aggregate(data.gva$total, 
                           by = list(Category = data.gva$state), FUN = sum)


victims.city <- aggregate(data.gva$total, 
                          by = list(Category = data.gva$citystate), 
                          FUN = sum)
victims.city$diff <- abs(victims.city$x - victims.city$x[256])

victims.killed.city <- aggregate(data.gva$killed, 
                                 by = list(Category = data.gva$citystate), 
                                 FUN = sum)
victims.killed.city$diff <- abs(victims.killed.city$x - victims.killed.city$x[256])

victims.injured.city <- aggregate(data.gva$injured, by = list(Category = data.gva$citystate), 
                                  FUN = sum)
victims.injured.city$diff <- abs(victims.injured.city$x - victims.injured.city$x[256])

victims.year <- aggregate(data.gva$total, by = list(Category= data.gva$year), FUN = sum)

# map
events.state$state <- state.abb[match(events.state$state, state.name)]
events.state <- subset(events.state, events.state$count != 78)

plot_usmap(data = events.state, values = "count", color = "grey") + 
  scale_fill_continuous(name = "Legend title here", label = scales::comma) +  
  theme(legend.position = "right")

