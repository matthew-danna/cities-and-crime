# Step 0
install.packages('explore')
install.packages('tidyverse')
library(explore)
library(tidyverse)

# Step 1
setwd("C:/Users/danna/Downloads/hate_crime")
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
unique(hate.dc$agency_type_name)

hate.similar <- hate %>%
  filter(population_group_code == '1B')

unique(hate.similar$pug_agency_name)

# Step 5
hate.events.sum <- hate.similar %>%
  group_by(pug_agency_name) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

hate.events.sum$diff <- hate.events.sum$count - hate.events.sum$count[34]
hate.events.sum$diff.abs <- abs(hate.events.sum$diff)

events <- hate.events.sum[order(hate.events.sum$diff.abs),]
events$rank.events <- seq.int(nrow(events)) - 1
