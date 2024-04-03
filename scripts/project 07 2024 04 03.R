# step 0: libraries
install.packages('tidyverse')
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

#### IF SF IS NOT INSTALLING CORRECTLY AND PROJECT 5 IS STILL IN YOUR ENVIRONMENT, DO THIS:
detach("package: rnaturalearth", unload = TRUE)
unloadNamespace("sf")

# step 1: get data
wapo.data <- read.csv("https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/v2/fatal-police-shootings-data.csv", 
                      stringsAsFactors = FALSE)

# step 2: cleaning data
wapo.data$date <- as.Date(wapo.data$date)
wapo.data$month <- substr(wapo.data$date, 6, 7)
wapo.data$year <- substr(wapo.data$date, 0, 4)
wapo.data$yearmonth <- paste(wapo.data$year, wapo.data$month, sep = "-")
wapo.data$statecity <- paste(wapo.data$state, wapo.data$city, sep = "-")

unique(wapo.data$race)
wapo.data$race <- gsub("W;B;N", "O", wapo.data$race)
wapo.data$race <- gsub("N;H", "O", wapo.data$race)
wapo.data$race <- gsub("W;H", "O", wapo.data$race)
wapo.data$race <- gsub("B;H", "O", wapo.data$race)
wapo.data$race <- gsub("W;B", "O", wapo.data$race)
unique(wapo.data$race)

wapo.data.clean <- subset(wapo.data, !is.na(wapo.data$latitude))
wapo.data.clean$latitude <- round(as.numeric(wapo.data.clean$latitude),4)
wapo.data.clean$longitude <- round(as.numeric(wapo.data.clean$longitude),4)
wapo.data.clean <- subset(wapo.data.clean, wapo.data.clean$longitude > -160)

# step 3: summary tables
wapo.race <- wapo.data %>% 
  group_by(race) %>% 
  summarise(count = n()) %>% 
  mutate(PCT = count/sum(count)*100)

wapo.year <- wapo.data %>% 
  group_by(year) %>% 
  summarise(count = n()) %>% 
  mutate(PCT = count/sum(count)*100)

wapo.race.mental <- wapo.data %>% 
  group_by(race, was_mental_illness_related) %>% 
  summarise(count = n()) %>% 
  mutate(pct = count/sum(count)*100)

wapo.race.city <- wapo.data %>% 
  group_by(statecity, race) %>% 
  summarise(count = n()) %>% 
  mutate(pct = count/sum(count)*100)

# step 4: maps!
world <- ne_countries(scale = "medium", returnclass = "sf") # this builds a list of countries
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) # this cleans up the US states

ggplot(data = world) + 
  geom_sf() + 
  geom_point(data = wapo.data, aes(x = longitude, y = latitude), size = 2, shape = 23, 
             fill = "black") + 
  coord_sf(xlim = c(-135, -60), ylim = c(25, 50), expand = FALSE)

ggplot(data = world) + 
  geom_sf() + 
  geom_sf(data = states, fill = NA) + 
  geom_point(data = wapo.data, aes(x = longitude, y = latitude), 
             size = 2, shape = 1, fill = "darkred") + 
  coord_sf(xlim = c(-135, -60), ylim = c(25, 50), expand = FALSE)

ggplot(data = world) + 
  geom_sf() + 
  geom_sf(data = states, fill = NA) + 
  geom_point(data = wapo.data, aes(x = longitude, y = latitude), size = 2, alpha = 0.05) + 
  coord_sf(xlim = c(-135, -60), ylim = c(25, 50), expand = FALSE)

ggplot(data = world) + 
  geom_sf() + 
  geom_sf(data = states, fill = NA) + 
  geom_point(data = wapo.data, aes(x = longitude, y = latitude), size = 1.5, alpha = 0.02) + 
  coord_sf(xlim = c(-135, -60), ylim = c(25, 50), expand = FALSE)

ggplot(data = world) + 
  geom_sf() + 
  geom_sf(data = states, fill = NA) + 
  stat_density_2d(data = wapo.data.clean, 
                  aes(x = longitude, y = latitude, fill = stat(level)), 
                  bins = 10, geom = "polygon") + 
  coord_sf(xlim = c(-135, -60), ylim = c(25, 50), expand = FALSE)

ggplot(data = world) + 
  geom_sf() + 
  geom_sf(data = states, fill = NA) + 
  stat_density_2d(data = wapo.data.clean, 
                  aes(x = longitude, y = latitude, fill = stat(level)), 
                  bins = 4, geom = "polygon") + 
  coord_sf(xlim = c(-135, -60), ylim = c(25, 50), expand = FALSE)

ggplot(data = world) + 
  geom_sf() + 
  geom_sf(data = states, fill = NA) + 
  geom_density_2d(data = wapo.data.clean, aes(x = longitude, y = latitude), bins = 5) + 
  coord_sf(xlim = c(-135, -60), ylim = c(25, 50), expand = FALSE)

ggplot(data = world) + 
  geom_sf() + 
  geom_sf(data = states, fill = NA) + 
  stat_density_2d(data = wapo.data.clean, aes(x = longitude, y = latitude, 
                                              fill = stat(level)), bins = 4, 
                  geom = "polygon") + 
  coord_sf(xlim = c(-135, -60), ylim = c(25, 50), expand = FALSE) + 
  facet_wrap(~ year, nrow = 3)

leaflet(wapo.data.clean) %>%
  addTiles() %>%
  addMarkers(lng = ~longitude, lat = ~latitude, clusterOptions = markerClusterOptions())
