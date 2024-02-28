# Step 0
library(explore)
library(tidyverse)

# Step 1
setwd("/Users/matthewdanna/Downloads") #Mac
hate <- read.csv("hate_crime.csv", stringsAsFactors = FALSE)

# Step 2
colnames(hate)
unique(hate$agency_type_name)
unique(hate$population_group_description)

# Step 3
hate.dc <- subset(hate, hate$state_abbr == 'DC')

# Step 4
hate.city <- hate %>% 
  filter(agency_type_name == 'City') 
hate.city.similar <- hate %>% 
  filter(agency_type_name == 'City' & 
           population_group_code == '1B')
unique(hate.city.similar$pug_agency_name)

# Step 5: event count analysis
sum.events <- hate.city.similar %>% 
  group_by(pug_agency_name) %>%
  summarise(count = n()) %>% 
  mutate(pct = count/sum(count)*100)

sum.events$diff <- sum.events$count - sum.events$count[34]
sum.events$abs.diff <- abs(sum.events$diff)
events <- sum.events[order(sum.events$abs.diff),]
events$rank <- seq.int(nrow(events)) - 1

# Step 6: offender race analysis
explore(hate.dc) # remember to close when done

sum.offender.race <- hate.city.similar %>% 
  group_by(pug_agency_name, offender_race) %>% 
  summarise(count = n()) %>% 
  mutate(pct = count/sum(count)*100)

offender.black <- subset(sum.offender.race, 
                         sum.offender.race$offender_race == 'Black or African American')
offender.black$diff <- offender.black$pct - offender.black$pct[33]
offender.black$abs.diff <- abs(offender.black$diff)
offender.black <- offender.black[order(offender.black$abs.diff),]
offender.black$rank <- seq.int(nrow(offender.black)) -1

offender.white <- subset(sum.offender.race, sum.offender.race$offender_race == 'White')
offender.white$diff <- offender.white$pct - offender.white$pct[34] # replace vv with the row number for D.C.
offender.white$abs.diff <- abs(offender.white$diff)
offender.white <- offender.white[order(offender.white$abs.diff),]
offender.white$rank <- seq.int(nrow(offender.white)) -1

offender.race <- rbind(offender.black, offender.white)

# Step 7a: location analysis
explore(hate.dc) # remember to close when done

sum.location <- hate.city.similar %>% 
  group_by(pug_agency_name, location_name) %>% 
  summarise(count = n()) %>% 
  mutate(pct = count/sum(count)*100)

loc.highway <- subset(sum.location, sum.location$location_name == 'Highway/Road/Alley/Street/Sidewalk')
loc.highway$diff <- loc.highway$pct - loc.highway$pct[33] 
loc.highway$abs.diff <- abs(loc.highway$diff)
loc.highway <- loc.highway[order(loc.highway$abs.diff),]
loc.highway$rank <- seq.int(nrow(loc.highway)) -1

loc.home <- subset(sum.location, sum.location$location_name == 'Residence/Home')
loc.home$diff <- loc.home$pct - loc.home$pct[33] 
loc.home$abs.diff <- abs(loc.home$diff)
loc.home <- loc.home[order(loc.home$abs.diff),]
loc.home$rank <- seq.int(nrow(loc.home)) -1

location <- rbind(loc.highway, loc.home)

# Step 7b: bias analysis
explore(hate.dc) # remember to close when done

sum.bias <- hate.city.similar %>% 
  group_by(pug_agency_name, bias_desc) %>% 
  summarise(count = n()) %>% 
  mutate(pct = count/sum(count)*100)

bias.ag <- subset(sum.bias, sum.bias$bias_desc == 'Anti-Gay (Male)')
bias.ag$diff <- bias.ag$pct - bias.ag$pct[32] 
bias.ag$abs.diff <- abs(bias.ag$diff)
bias.ag <- bias.ag[order(bias.ag$abs.diff),]
bias.ag$rank <- seq.int(nrow(bias.ag)) -1

bias.ab <- subset(sum.bias, sum.bias$bias_desc == 'Anti-Black or African American')
bias.ab$diff <- bias.ab$pct - bias.ab$pct[34] 
bias.ab$abs.diff <- abs(bias.ab$diff)
bias.ab <- bias.ab[order(bias.ab$abs.diff),]
bias.ab$rank <- seq.int(nrow(bias.ab)) -1

bias.at <- subset(sum.bias, sum.bias$bias_desc == 'Anti-Transgender')
bias.at$diff <- bias.at$pct - bias.at$pct[24] 
bias.at$abs.diff <- abs(bias.at$diff)
bias.at <- bias.at[order(bias.at$abs.diff),]
bias.at$rank <- seq.int(nrow(bias.at)) -1

bias <- rbind(bias.ab, bias.ag, bias.at)

##### this process creates 4 tables to analyze for similarity:
##### events
##### offender.race
##### location
##### bias


