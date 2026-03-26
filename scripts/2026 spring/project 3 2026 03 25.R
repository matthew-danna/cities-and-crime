# Step 0
install.packages('leaflet')
install.packages(c("cowplot", "ggrepel", "rgeos", "sf", "maps", "usmap", "ggspatial", "libwgeom", "rnaturalearth", "rnaturalearthdata"))

library(tidyverse)
library(leaflet)
library(rnaturalearth)
library(rnaturalearthdata)
library(maps)
library(lubridate)
library(usmap)
library(sf)
theme_set(theme_bw())

# Step 1
wapo.data <- read.csv("https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/v2/fatal-police-shootings-data.csv", stringsAsFactors = FALSE)

# Step 2
wapo.data$date <- as.Date(wapo.data$date)
wapo.data$year <- substr(wapo.data$date, 0, 4)

wapo.data$city <- gsub("Bronx", "New York", wapo.data$city)
wapo.data$city <- gsub("Brooklyn", "New York", wapo.data$city)
wapo.data$city <- gsub("Manhattan", "New York", wapo.data$city)
wapo.data$city <- gsub("Queens", "New York", wapo.data$city)
wapo.data$city <- gsub("Staten Island", "New York", wapo.data$city)

wapo.data$statecity <- paste(wapo.data$state, wapo.data$city, sep = "-")

wapo.data$race <- gsub("W;B;N", "W", wapo.data$race)
wapo.data$race <- gsub("N;H", "N", wapo.data$race)
wapo.data$race <- gsub("W;H", "W", wapo.data$race)
wapo.data$race <- gsub("B;H", "B", wapo.data$race)
wapo.data$race <- gsub("W;B", "W", wapo.data$race)
wapo.data$race <- gsub("W;A", "W", wapo.data$race)

wapo.data.map <- subset(wapo.data, !is.na(wapo.data$latitude))

# Step 3
colnames(wapo.data)

wapo.city <- wapo.data %>% 
  group_by(statecity) %>% 
  summarise(count = n()) %>% 
  mutate(pct = round(count/sum(count)*100, 2))

wapo.flee <- wapo.data %>% 
  group_by(flee_status) %>% 
  summarise(count = n()) %>% 
  mutate(pct = round(count/sum(count)*100, 2))

wapo.armed <- wapo.data %>% 
  group_by(armed_with) %>% 
  summarise(count = n()) %>% 
  mutate(pct = round(count/sum(count)*100, 2))

wapo.gender <- wapo.data %>% 
  group_by(gender) %>% 
  summarise(count = n()) %>% 
  mutate(pct = round(count/sum(count)*100, 2))

wapo.race <- wapo.data %>% 
  group_by(race) %>% 
  summarise(count = n()) %>% 
  mutate(pct = round(count/sum(count)*100, 2))

wapo.mental <- wapo.data %>% 
  group_by(was_mental_illness_related) %>% 
  summarise(count = n()) %>% 
  mutate(pct = round(count/sum(count)*100, 2))

wapo.camera <- wapo.data %>% 
  group_by(body_camera) %>% 
  summarise(count = n()) %>% 
  mutate(pct = round(count/sum(count)*100, 2))

# Step 4
project3.cities <- data.frame(
  city = c(
    "Austin", "Baltimore", "Boston", "Charlotte", "Chicago",
    "Cleveland", "Columbus", "Dallas", "Denver", "Detroit",
    "El Paso", "Houston", "Indianapolis", "Kansas City",
    "Las Vegas", "Long Beach", "Los Angeles", "Louisville",
    "Memphis", "Milwaukee", "Minneapolis", "Nashville",
    "New York", "Oakland", "Oklahoma City", "Philadelphia",
    "Phoenix", "Portland", "San Antonio", "San Diego",
    "San Francisco", "San Jose", "Seattle", "Tucson",
    "Washington"
  ),
  state = c(
    "TX", "MD", "MA", "NC", "IL",
    "OH", "OH", "TX", "CO", "MI",
    "TX", "TX", "IN", "MO",
    "NV", "CA", "CA", "KY",
    "TN", "WI", "MN", "TN",
    "NY", "CA", "OK", "PA",
    "AZ", "OR", "TX", "CA",
    "CA", "CA", "WA", "AZ",
    "DC"
  ),
  stringsAsFactors = FALSE
)

project3.cities$statecity <- paste(project3.cities$state,
                                   project3.cities$city,
                                   sep = "-")
project3 <- project3.cities %>%
  left_join(wapo.data, by = 'statecity')

# Step 5
### total events
project3.events <- project3 %>%
  group_by(statecity) %>%
  summarise(count = n())
  
project3.events$diff <- abs(project3.events$count - 
  project3.events$count[10])
project3.events <- project3.events[order(project3.events$diff),]
project3.events$rank.event <- seq.int(nrow(project3.events))-1

### flee status
project3.flee <- project3 %>%
  group_by(statecity, flee_status) %>%
  summarise(count = n()) %>%
  mutate(pct.flee = round(count/sum(count)*100, 2))

# open the table, filter to DC
# find the flee status that is the most significant
# then filter the table based on that status

project3.flee <- subset(project3.flee, 
                        project3.flee$flee_status == 'not')

project3.flee$diff <- abs(project3.flee$pct.flee - 
                              project3.flee$pct.flee[10])
project3.flee <- project3.flee[order(project3.flee$diff),]
project3.flee$rank.flee <- seq.int(nrow(project3.flee))-1

### armed with
project3.armed <- project3 %>%
  group_by(statecity, armed_with) %>%
  summarise(count = n()) %>%
  mutate(pct.armed = round(count/sum(count)*100, 2))

# open the table, filter to DC
# find the armed with that is the most significant
# then filter the table based on that 

project3.armed <- subset(project3.armed, 
                        project3.armed$armed_with == 'gun')

project3.armed$diff <- abs(project3.armed$pct.armed - 
                            project3.armed$pct.armed[10])
project3.armed <- project3.armed[order(project3.armed$diff),]
project3.armed$rank.armed <- seq.int(nrow(project3.armed))-1

### gender
project3.gender <- project3 %>%
  group_by(statecity, gender) %>%
  summarise(count = n()) %>%
  mutate(pct.gender = round(count/sum(count)*100, 2))

project3.gender <- subset(project3.gender, 
                         project3.gender$gender == 'male')

project3.gender$diff <- abs(project3.gender$pct.gender - 
                             project3.gender$pct.gender[10])
project3.gender <- project3.gender[order(project3.gender$diff),]
project3.gender$rank.gender <- seq.int(nrow(project3.gender))-1

### race
project3.race <- project3 %>%
  group_by(statecity, race) %>%
  summarise(count = n()) %>%
  mutate(pct.race = round(count/sum(count)*100, 2))

project3.race <- subset(project3.race, 
                          project3.race$race == 'B')

project3.race$diff <- abs(project3.race$pct.race - 
                              project3.race$pct.race[10])
project3.race <- project3.race[order(project3.race$diff),]
project3.race$rank.race <- seq.int(nrow(project3.race))-1

### mental illness
project3.mental <- project3 %>%
  group_by(statecity, was_mental_illness_related) %>%
  summarise(count = n()) %>%
  mutate(pct.mental = round(count/sum(count)*100, 2))

project3.mental <- subset(project3.mental, 
                        project3.mental$was_mental_illness_related == 'False')

project3.mental$diff <- abs(project3.mental$pct.mental - 
                            project3.mental$pct.mental[10])
project3.mental <- project3.mental[order(project3.mental$diff),]
project3.mental$rank.mental <- seq.int(nrow(project3.mental))-1

### body camera
project3.camera <- project3 %>%
  group_by(statecity, body_camera) %>%
  summarise(count = n()) %>%
  mutate(pct.camera = round(count/sum(count)*100, 2))

project3.camera <- subset(project3.camera, 
                          project3.camera$body_camera == 'False')

project3.camera$diff <- abs(project3.camera$pct.camera - 
                              project3.camera$pct.camera[10])
project3.camera <- project3.camera[order(project3.camera$diff),]
project3.camera$rank.camera <- seq.int(nrow(project3.camera))-1

# Step 6
### Merge all the ranking tables into one
sub.events <- project3.events %>% select(statecity, count, rank.event)
sub.flee <- project3.flee %>% select(statecity, pct.flee, rank.flee)
sub.armed <- project3.armed %>% select(statecity, pct.armed, rank.armed)
sub.gender <- project3.gender %>% select(statecity, pct.gender, rank.gender)
sub.race <- project3.race %>% select(statecity, pct.race, rank.race)
sub.mental <- project3.mental %>% select(statecity, pct.mental, rank.mental)
sub.camera <- project3.camera %>% select(statecity, pct.camera, rank.camera)

### join them all
project3.combined <- list(
  sub.events,
  sub.flee,
  sub.armed,
  sub.gender,
  sub.race,
  sub.mental,
  sub.camera
) %>% reduce(full_join, by = "statecity")

# if you want to adjust where the columns are
test <- project3.combined[c(1:10,13,11,12)]

# Step 7: Graphs
### nah, use Project 2 scatter plots or something similar

# Step 8: Maps!
world <- ne_countries(scale = "medium", returnclass = "sf") # this builds a list of countries
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) # this cleans up the US states

# transparent point map
ggplot(data = world) + 
  geom_sf() + 
  geom_sf(data = states, fill = NA) + 
  geom_point(data = wapo.data.map, aes(x = longitude, y = latitude), 
             size = 2, alpha = 0.05) + 
  coord_sf(xlim = c(-135, -60), ylim = c(25, 50), expand = FALSE)

ggplot(data = world) + 
  geom_sf() + 
  geom_sf(data = states, fill = NA) + 
  geom_point(data = wapo.data.map, aes(x = longitude, y = latitude), 
             size = 2, alpha = 0.05) + 
  coord_sf(xlim = c(-135, -60), ylim = c(25, 50), expand = FALSE) + 
  facet_wrap(~ year, nrow = 3)

ggplot(data = world) + 
  geom_sf() + 
  geom_sf(data = states, fill = NA) + 
  geom_point(data = wapo.data.map, aes(x = longitude, y = latitude), 
             size = 2, alpha = 0.05) + 
  coord_sf(xlim = c(-135, -60), ylim = c(25, 50), expand = FALSE) + 
  facet_wrap(~ race, nrow = 3)

# hex maps
ggplot(data = world) + 
  geom_sf() + 
  geom_sf(data = states, fill = NA) + 
  geom_hex(aes(x = longitude, y = latitude), data = wapo.data.map, 
           bins = 45, alpha = 0.6) + 
  scale_fill_continuous(type = "viridis") + 
  coord_sf(xlim = c(-135, -60), ylim = c(25, 50), expand = FALSE)

# an example using just the 35 cities
ggplot(data = world) + 
  geom_sf() + 
  geom_sf(data = states, fill = NA) + 
  geom_hex(aes(x = longitude, y = latitude), data = project3, 
           bins = 25, alpha = 0.6) + 
  scale_fill_continuous(type = "viridis") + 
  coord_sf(xlim = c(-135, -60), ylim = c(25, 50), expand = FALSE)

# interactive map
leaflet(wapo.data.map) %>%
  addTiles() %>%
  addMarkers(lng = ~longitude, lat = ~latitude, 
             clusterOptions = markerClusterOptions())



