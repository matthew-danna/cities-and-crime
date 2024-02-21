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
explore(hate.dc)

# Step 4
hate.city <- hate %>% 
  filter(agency_type_name == 'City') 
hate.city.similar <- hate %>% 
  filter(agency_type_name == 'City' & 
           population_group_code == '1B')
unique(hate.city.similar$pug_agency_name)

# Step 5
sum.events <- hate.city.similar %>% 
  group_by(pug_agency_name) %>%
  summarise(count = n()) %>% 
  mutate(pct = count/sum(count)*100)

sum.events$diff <- sum.events$count - sum.events$count[34]
sum.events$abs.diff <- abs(sum.events$diff)
events <- sum.events[order(sum.events$abs.diff),]
events$rank <- seq.int(nrow(events)) - 1

