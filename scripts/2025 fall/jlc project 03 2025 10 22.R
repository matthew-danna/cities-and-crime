# Step 0
library(tidyverse)

# Step 1
setwd("C:/Users/danna/Downloads") # for windows
setwd("/Users/xxx/Downloads") # for a Mac
hate <- read.csv("hate_crime.csv", stringsAsFactors = FALSE)

# Step 4
city.filter <- c("Austin TX",
                 "Baltimore MD",
                 "Boston MA",
                 "Charlotte NC",
                 "Chicago IL",
                 "Cleveland OH",
                 "Columbus OH",
                 "Dallas TX",
                 "Denver CO",
                 "Detroit MI",
                 "El Paso TX",
                 "Houston TX",
                 "Indianapolis IN",
                 "Kansas City MO",
                 "Las Vegas NV",
                 "Long Beach CA",
                 "Los Angeles CA",
                 "Louisville KY",
                 "Memphis TN",
                 "Milwaukee WI",
                 "Minneapolis MN",
                 "Nashville TN",
                 "New York City NY",
                 "Oakland CA",
                 "Oklahoma City OK",
                 "Philadelphia PA",
                 "Phoenix AZ",
                 "Portland OR",
                 "San Antonio TX",
                 "San Diego CA",
                 "San Francisco CA",
                 "San Jose CA",
                 "Seattle WA",
                 "Tucson AZ",
                 "Washington DC")

hate$city.state <- paste(hate$pug_agency_name, hate$state_abbr,
                         sep = " ")

hate$city.state <- gsub("Charlotte-Mecklenburg NC", 
                        "Charlotte NC", hate$city.state)

hate$city.state <- gsub("Las Vegas Metropolitan Police Department NV",
                        "Las Vegas NV", hate$city.state)
hate$city.state <- gsub("City of Las Vegas Department of Public Safety NV",
                        "Las Vegas NV", hate$city.state)

hate$city.state <- gsub("Metropolitan Nashville Police Department TN",
                        "Nashville TN", hate$city.state)
hate$city.state <- gsub("Nashville State Community College TN",
                        "Nashville TN", hate$city.state)
hate$city.state <- gsub("Nashville International Airport TN",
                        "Nashville TN", hate$city.state)

hate$city.state <- gsub("New York NY", "New York City NY",
                        hate$city.state)

hate$city.state <- gsub("University of Louisville KY", "Louisville KY",
                        hate$city.state)
hate$city.state <- gsub("Louisville Metro KY", "Louisville KY",
                        hate$city.state)

hate.similar <- hate %>% filter(city.state %in% city.filter)

# Step 5 - EVENT COUNT
hate.events.sum <- hate.similar %>% 
  group_by(city.state) %>%
  summarise(count = n()) %>% 
  mutate(pct = round(count/sum(count)*100,2))

hate.events.sum$diff <- hate.events.sum$count - hate.events.sum$count[35] 
hate.events.sum$abs.diff <- abs(hate.events.sum$diff)
events <- hate.events.sum[order(hate.events.sum$abs.diff),]
events$rank <- seq.int(nrow(events)) - 1

# Step 6 - OFFENDER RACE
sum.offender.race <- hate.similar %>% 
  group_by(city.state, offender_race) %>% 
  summarise(count = n()) %>% 
  mutate(pct = round(count/sum(count)*100,2))

offender.black <- subset(sum.offender.race, 
                         sum.offender.race$offender_race == 'Black or African American')
offender.black$diff <- offender.black$pct - offender.black$pct[35] # replace ww with the row number for D.C.
offender.black$abs.diff <- abs(offender.black$diff)
offender.black <- offender.black[order(offender.black$abs.diff),]
offender.black$rank <- seq.int(nrow(offender.black)) -1

offender.white <- subset(sum.offender.race, sum.offender.race$offender_race == 'White')
offender.white$diff <- offender.white$pct - offender.white$pct[35] # replace vv with the row number for D.C.
offender.white$abs.diff <- abs(offender.white$diff)
offender.white <- offender.white[order(offender.white$abs.diff),]
offender.white$rank <- seq.int(nrow(offender.white)) -1

offender.race <- rbind(offender.black, offender.white)

# Step 7 - LOCATION
sum.location <- hate.similar %>% 
  group_by(city.state, location_name) %>% 
  summarise(count = n()) %>% 
  mutate(pct = round(count/sum(count)*100,2))

loc.highway <- subset(sum.location, sum.location$location_name == 'Highway/Road/Alley/Street/Sidewalk')
loc.highway$diff <- loc.highway$pct - loc.highway$pct[35] 
loc.highway$abs.diff <- abs(loc.highway$diff)
loc.highway <- loc.highway[order(loc.highway$abs.diff),]
loc.highway$rank <- seq.int(nrow(loc.highway)) - 1

loc.home <- subset(sum.location, sum.location$location_name == 'Residence/Home')
loc.home$diff <- loc.home$pct - loc.home$pct[35] 
loc.home$abs.diff <- abs(loc.home$diff)
loc.home <- loc.home[order(loc.home$abs.diff),]
loc.home$rank <- seq.int(nrow(loc.home)) - 1

location <- rbind(loc.highway, loc.home)

# BIAS
sum.bias <- hate.similar %>%
  group_by(city.state, bias_desc) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

# open the table, filter to DC, sort by PCT, find the most interesting results
bias.ag <- subset(sum.bias, sum.bias$bias_desc == 'Anti-Gay (Male)')
bias.ag$diff <- bias.ag$pct - bias.ag$pct[35]
bias.ag$abs.diff <- abs(bias.ag$diff)
bias <- bias.ag[order(bias.ag$abs.diff),]
bias$rank <- seq.int(nrow(bias)) - 1

# VICTIM TYPE
sum.victim <- hate.similar %>%
  group_by(city.state, victim_types) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

victim <- subset(sum.victim, sum.victim$victim_types == 'Individual')
victim$diff <- victim$pct - victim$pct[35]
victim$abs.diff <- abs(victim$diff)
victim <- victim[order(victim$abs.diff),]
victim$rank <- seq.int(nrow(victim)) - 1

# YEAR (RECENT YEARS)
sum.year <- hate.similar %>%
  group_by(city.state, data_year) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

hate.recent <- subset(hate.similar, hate.similar$data_year >= '2016')

sum.recent <- hate.recent %>%
  group_by(city.state) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

sum.recent$diff <- sum.recent$count - sum.recent$count[35]
sum.recent$abs.diff <- abs(sum.recent$diff)
recent <- sum.recent[order(sum.recent$abs.diff),]
recent$rank <- seq.int(nrow(recent)) - 1
