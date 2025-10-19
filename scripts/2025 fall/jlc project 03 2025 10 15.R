# Step 0
library(tidyverse)

# Step 1
setwd("C:/Users/danna/Downloads") # for windows
setwd("/Users/xxx/Downloads") # for a Mac
hate <- read.csv("hate_crime.csv", stringsAsFactors = FALSE)

# Step 2
colnames(hate)
unique(hate$agency_type_name)
unique(hate$population_group_description)
unique(hate$pug_agency_name)

# Step 3
hate.dc <- subset(hate, hate$state_abbr == 'DC')

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

hate.similar <- hate %>% filter(city.state %in% city.filter)
unique(hate.similar$city.state)

# Step 5
hate.events.sum <- hate.similar %>% 
  group_by(city.state) %>%
  summarise(count = n()) %>% 
  mutate(pct = round(count/sum(count)*100,2))

hate.events.sum$diff <- hate.events.sum$count - hate.events.sum$count[35] 
hate.events.sum$abs.diff <- abs(hate.events.sum$diff)
events <- hate.events.sum[order(hate.events.sum$abs.diff),]
events$rank <- seq.int(nrow(events)) - 1

