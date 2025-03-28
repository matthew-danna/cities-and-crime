# Step 0
install.packages('explore')
install.packages('tidyverse')
library(explore)
library(tidyverse)

# Step 1
setwd("C:/Users/danna/Downloads")
hate <- read.csv("hate_crime.csv", stringsAsFactors = FALSE)

# Step 2
colnames(hate)
unique(hate$agency_type_name)
unique(hate$population_group_description)

# Step 3
hate.dc <- subset(hate, hate$state_abbr == 'DC')

explore(hate)
explore(hate.dc)

# Step 4
hate.similar <- hate %>%
  filter(population_group_code == '1B')

unique(hate.similar$pug_agency_name)

# Step 5
### events
hate.events.sum <- hate.similar %>%
  group_by(pug_agency_name) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

hate.events.sum$diff <- abs(hate.events.sum$count - hate.events.sum$count[34])

events <- hate.events.sum[order(hate.events.sum$diff),]
events$rank.events <- seq.int(nrow(events)) - 1

### offender race
# 1. create the summary table
hate.offender.race <- hate.similar %>%
  group_by(pug_agency_name, offender_race) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))
# 2. filter by the field most unique to DC
offender.race <- subset(hate.offender.race, 
                        hate.offender.race$offender_race == 'Black or African American')
# 3. calculate the difference between DC and the other cities
offender.race$diff <- abs(offender.race$pct - offender.race$pct[33])

# 4. reorder the table
offender.race <- offender.race[order(offender.race$diff),]
# 5. rank the table
offender.race$rank.race <- seq.int(nrow(offender.race)) - 1

### offense type
# 1. create the summary table
hate.offense <- hate.similar %>%
  group_by(pug_agency_name, offense_name) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))
# 2. filter by the field most unique to DC
offense <- subset(hate.offense, 
                        hate.offense$offense_name == 'Simple Assault')
# 3. calculate the difference between DC and the other cities
offense$diff <- abs(offense$pct - offense$pct[34])
# 4. reorder the table
offense <- offense[order(offense$diff),]
# 5. rank the table
offense$rank.offense <- seq.int(nrow(offense)) - 1

### location name
# 1. create the summary table
hate.location <- hate.similar %>%
  group_by(pug_agency_name, location_name) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))
# 2. filter by the field most unique to DC
location <- subset(hate.location, 
                  hate.location$location_name == 'Highway/Road/Alley/Street/Sidewalk')
# 3. calculate the difference between DC and the other cities
location$diff <- abs(location$pct - location$pct[34])
# 4. reorder the table
location <- location[order(location$diff),]
# 5. rank the table
location$rank.location <- seq.int(nrow(location)) - 1

### bias
# 1. create the summary table
hate.bias <- hate.similar %>%
  group_by(pug_agency_name, bias_desc) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))
# 2. filter by the field most unique to DC
bias <- subset(hate.bias, 
                   hate.bias$bias_desc == 'Anti-Gay (Male)')
# 3. calculate the difference between DC and the other cities
bias$diff <- abs(bias$pct - bias$pct[32])
# 4. reorder the table
bias <- bias[order(bias$diff),]
# 5. rank the table
bias$rank.bias <- seq.int(nrow(bias)) - 1

### victim
# 1. create the summary table
hate.victim <- hate.similar %>%
  group_by(pug_agency_name, victim_types) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))
# 2. filter by the field most unique to DC
victim <- subset(hate.victim, 
               hate.victim$victim_types == 'Individual')
# 3. calculate the difference between DC and the other cities
victim$diff <- abs(victim$pct - victim$pct[34])
# 4. reorder the table
victim <- victim[order(victim$diff),]
# 5. rank the table
victim$rank.victim <- seq.int(nrow(victim)) - 1

### combine all the tables
# make consistent column names
names(events) <- c("City", "Count.Events", "PCT.Events", "Diff.Events", "Rank.Events")
names(offender.race) <- c("City", "Race", "Count.Race", "PCT.Race", "Diff.Race", "Rank.Race")
names(offense) <- c("City", "Offense", "Count.Offense", "PCT.Offense", "Diff.Offense", "Rank.Offense")
names(location) <- c("City", "Location", "Count.Location", "PCT.Location", "Diff.Location", "Rank.Location")
names(bias) <- c("City", "Bias", "Count.Bias", "PCT.Bias", "Diff.Bias", "Rank.Bias")
names(victim) <- c("City", "Victim", "Count.Victim", "PCT.Victim", "Diff.Victim", "Rank.Victim")

# joins
hate.ranks <- events %>%
  left_join(offender.race, by = 'City')
hate.ranks <- hate.ranks %>%
  left_join(offense, by = 'City')
hate.ranks <- hate.ranks %>%
  left_join(location, by = 'City')
hate.ranks <- hate.ranks %>%
  left_join(bias, by = 'City')
hate.ranks <- hate.ranks %>%
  left_join(victim, by = 'City')

hate.ranks$Average <- mean(hate.ranks$Rank.Events, hate.ranks$Rank.Race, hate.ranks$Rank.Location,
                           hate.ranks$Rank.Offense, hate.ranks$Rank.Victim, hate.ranks$Rank.Bias)

ranks <- hate.ranks[c(1,5,10,15,20,25,30)]

