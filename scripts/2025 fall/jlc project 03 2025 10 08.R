# Step 0
library(tidyverse)
library(explore)

# Step 1
setwd("C:/Users/xxx/Downloads")
hate <- read.csv("hate_crime.csv", stringsAsFactors = FALSE)

# Step 2
colnames(hate)
unique(hate$agency_type_name)
unique(hate$population_group_description)

# Step 3
hate.dc <- subset(hate, hate$state_abbr == 'DC')
explore(hate)
explore(hate.dc)
