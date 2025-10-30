# Step 0
install.packages('fmsb')
library(tidyverse)
library(fmsb)
library(scales)

# Step 1
setwd("C:/Users/xxx/Downloads") # for windows
setwd("/Users/xxx/Downloads/hate_crime/hate_crime") # for a Mac
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
events$rank.events <- seq.int(nrow(events)) - 1

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
offender.black$rank.off.aa <- seq.int(nrow(offender.black)) -1

offender.white <- subset(sum.offender.race, sum.offender.race$offender_race == 'White')
offender.white$diff <- offender.white$pct - offender.white$pct[35] # replace vv with the row number for D.C.
offender.white$abs.diff <- abs(offender.white$diff)
offender.white <- offender.white[order(offender.white$abs.diff),]
offender.white$rank.off.w <- seq.int(nrow(offender.white)) -1

# Step 7 - LOCATION
sum.location <- hate.similar %>% 
  group_by(city.state, location_name) %>% 
  summarise(count = n()) %>% 
  mutate(pct = round(count/sum(count)*100,2))

loc.highway <- subset(sum.location, sum.location$location_name == 'Highway/Road/Alley/Street/Sidewalk')
loc.highway$diff <- loc.highway$pct - loc.highway$pct[35] 
loc.highway$abs.diff <- abs(loc.highway$diff)
loc.highway <- loc.highway[order(loc.highway$abs.diff),]
loc.highway$rank.road <- seq.int(nrow(loc.highway)) - 1

loc.home <- subset(sum.location, sum.location$location_name == 'Residence/Home')
loc.home$diff <- loc.home$pct - loc.home$pct[35] 
loc.home$abs.diff <- abs(loc.home$diff)
loc.home <- loc.home[order(loc.home$abs.diff),]
loc.home$rank.home <- seq.int(nrow(loc.home)) - 1

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
bias$rank.bias <- seq.int(nrow(bias)) - 1

# VICTIM TYPE
sum.victim <- hate.similar %>%
  group_by(city.state, victim_types) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

victim <- subset(sum.victim, sum.victim$victim_types == 'Individual')
victim$diff <- victim$pct - victim$pct[35]
victim$abs.diff <- abs(victim$diff)
victim <- victim[order(victim$abs.diff),]
victim$rank.victim <- seq.int(nrow(victim)) - 1

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
recent$rank.recent <- seq.int(nrow(recent)) - 1

# COMBINE

# Select only city and rank columns from each table
events.sub <- events %>% select(city.state, rank.events)
offender.black.sub <- offender.black %>% select(city.state, rank.off.aa)
offender.white.sub <- offender.white %>% select(city.state, rank.off.w)
loc.highway.sub <- loc.highway %>% select(city.state, rank.road)
loc.home.sub <- loc.home %>% select(city.state, rank.home)
bias.sub <- bias %>% select(city.state, rank.bias)
victim.sub <- victim %>% select(city.state, rank.victim)
recent.sub <- recent %>% select(city.state, rank.recent)

# Combine all tables using full joins
rankings.combined <- list(
  events.sub,
  offender.black.sub,
  offender.white.sub,
  loc.highway.sub,
  loc.home.sub,
  bias.sub,
  victim.sub,
  recent.sub
) %>%
  reduce(full_join, by = "city.state")

# sum of ranks
rankings.combined <- rankings.combined %>%
  mutate(rank.total = rowSums(across(starts_with("rank.")), na.rm = TRUE)) %>%
  arrange(rank.total)

# RADAR CHART
# Select DC and top x most similar cities (lowest total_rank)
# 1 of 2 ways to filter data for your graph
radar.data <- rankings.combined %>%
  arrange(rank.total) %>%
  slice(1:2) %>%
  select(city.state, starts_with("rank."))

# 2 of 2 ways to filter data for your graph
radar.data <- subset(rankings.combined, 
                     rankings.combined$city.state == 'Washington DC' |
                       rankings.combined$city.state == 'Chicago IL' |
                       rankings.combined$city.state == 'El Paso TX')

# Prepare data for fmsb (it needs max/min rows at top)
radar.ready <- radar.data %>%
  select(-city.state) %>%
  as.data.frame()

radar.ready <- rbind(
  apply(radar.ready, 2, max, na.rm = TRUE),
  apply(radar.ready, 2, min, na.rm = TRUE),
  radar.ready
)

rownames(radar.ready) <- c("max", "min", radar.data$city.state)

# VISUALS
# Create radar chart
radarchart(
  radar.ready,
  axistype = 1,
  pcol = c("red", "blue", "green", "purple", "orange","grey"),
  pfcol = alpha(c("red", "blue", "green", "purple","orange","grey"), 0.3),
  plwd = 2,
  title = "Washington, D.C. vs Similar Cities (Ranking Comparison)"
)

legend(
  "bottomleft",
  legend = rownames(radar.ready)[-c(1, 2)],
  col = c("red", "blue", "green", "purple", "orange","grey"),
  lty = 1,
  bty = "n",
  cex = 0.5
)

# Bar graph
ggplot(rankings.combined,
       aes(x = reorder(city.state, rank.total), y = rank.total,
           fill = ifelse(city.state == "Washington DC", "Washington DC", "Other"))) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("Washington DC" = "red", "Other" = "grey70")) +
  labs(title = "Overall City Similarity to Washington, D.C.",
       x = "City",
       y = "Total Similarity Rank (lower = more similar)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

#### BRING IN PROJECT 2 HERE 
# this is only an example, and only has 2 of the 4 datasets required
data.rankings = data.frame(
  "City" = c(
    "Austin TX",
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
    "Washington DC"
  )
)

##### FIRST METRIC (Firearm Death Rate)
# Default all cities to 30
data.rankings$firearm.deaths = 30  

# Assign custom values
# these are based on manually determining similarity
data.rankings$firearm.deaths[data.rankings$City == "Indianapolis IN"] <- 2
data.rankings$firearm.deaths[data.rankings$City == "Philadelphia PA"] <- 3
data.rankings$firearm.deaths[data.rankings$City == "Louisville KY"]   <- 4
data.rankings$firearm.deaths[data.rankings$City == "Tucson AZ"]       <- 6
data.rankings$firearm.deaths[data.rankings$City == "Houston TX"]      <- 1
data.rankings$firearm.deaths[data.rankings$City == "Chicago IL"]      <- 5

# Assign 10s
# these are not the most similar, but the next tier
ten_cities <- c("Memphis TN", "Detroit MI", "Cleveland OH", 
                "Milwaukee WI", "Baltimore MD", "Kansas City MO")
data.rankings$firearm.deaths[data.rankings$City %in% ten_cities] <- 10

# Assign Washington DC = 0
# DC gets assigned a 0 for every dataset
data.rankings$firearm.deaths[data.rankings$City == "Washington DC"] <- 0

##### SECOND METRIC
# BUCKET C
# Default all cities to 30
data.rankings$diabetes.deaths = 30  

# BUCKET A
# Assign custom values
# these are based on manually determining similarity
data.rankings$diabetes.deaths[data.rankings$City == "Seattle WA"] <- 1
data.rankings$diabetes.deaths[data.rankings$City == "New York City NY"] <- 2
data.rankings$diabetes.deaths[data.rankings$City == "Austin TX"]   <- 3
data.rankings$diabetes.deaths[data.rankings$City == "Kansas City MO"] <- 4
data.rankings$diabetes.deaths[data.rankings$City == "Columbus OH"]      <- 5
data.rankings$diabetes.deaths[data.rankings$City == "Boston MA"]      <- 6
data.rankings$diabetes.deaths[data.rankings$City == "San Francisco CA"] <- 7

# BUCKET B
# Assign 10s
# these are not the most similar, but the next tier
ten_cities <- c("Chicago IL", "Philadelphia PA", "Louisville KY", "Minneapolis MN",
                "San Diego CA", "Denver CO", "Las Vegas NV", "Oakland CA", "Charlotte NC",
                "San Jose CA", "Dallas TX")
data.rankings$diabetes.deaths[data.rankings$City %in% ten_cities] <- 10

# Assign Washington DC = 0
data.rankings$diabetes.deaths[data.rankings$City == "Washington DC"] <- 0

#### rename the city column in Project
names(data.rankings) <- c("city.state", "proj2.firearm", "proj2.diabetes", 
                          "proj2.xxx", "proj2.yyy")

### join project 2 and project 3 datasets
final.data <- data.rankings %>%
  left_join(rankings.combined)

final.data <- final.data[c(1:13)]
