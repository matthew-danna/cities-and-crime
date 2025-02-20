# STEP 0
library(tidyverse)
library(leaflet)
library(tidycensus)
library(rnaturalearth)
library(rnaturalearthdata)
library(maps)
library(lubridate)
library(usmap)
library(sf)
theme_set(theme_bw())

# STEP 1
wapo.data <- read.csv("https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/v2/fatal-police-shootings-data.csv", 
                      stringsAsFactors = FALSE)

# STEP 2
wapo.data$date <- as.Date(wapo.data$date)
wapo.data$month <- substr(wapo.data$date, 6, 7)
wapo.data$year <- substr(wapo.data$date, 0, 4)
wapo.data$yearmonth <- paste(wapo.data$year, wapo.data$month, sep = "-")
wapo.data$statecity <- paste(wapo.data$state, wapo.data$city, sep = "-")

unique(wapo.data$race)
# Option 1: leave them as is
# Option 2: label them all as Other
# Option 3: take the first value
# Option 4: all races as binary variables
wapo.data$race <- gsub("W;B;N", "O", wapo.data$race)
wapo.data$race <- gsub("N;H", "O", wapo.data$race)
wapo.data$race <- gsub("W;H", "O", wapo.data$race)
wapo.data$race <- gsub("B;H", "O", wapo.data$race)
wapo.data$race <- gsub("W;B", "O", wapo.data$race)
wapo.data$race <- gsub("W;A", "O", wapo.data$race)
unique(wapo.data$race)

wapo.data.map <- subset(wapo.data, !is.na(wapo.data$latitude))

# STEP 3
sum.race <- wapo.data %>%
  group_by(race) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

sum.city <- wapo.data %>%
  group_by(statecity) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

sum.year <- wapo.data %>%
  group_by(year) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

sum.mental <- wapo.data %>%
  group_by(was_mental_illness_related) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

sum.race.mental <- wapo.data %>%
  group_by(race, was_mental_illness_related) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

sum.city.race <- wapo.data %>%
  group_by(statecity, race) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

sum.city.year <- wapo.data %>%
  group_by(statecity, year) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

# STEP 4
ggplot(wapo.data) +
  geom_bar(aes(x = race), stat = "count", fill = "blue")

ggplot(sum.race.mental, aes(x = factor(race), y = pct, 
                            fill = factor(was_mental_illness_related))) + 
  geom_bar(stat="identity", width = 0.7) + 
  labs(title = "Police Shootings by Race and Mental Illness",
       x = "Race", y = "Percent", fill = "Mental Illness Related") + 
  theme_minimal(base_size = 14)

ggplot(wapo.data) + 
  geom_bar(aes(x = was_mental_illness_related), stat = "count", fill = "orange") + 
  facet_wrap(~ race, nrow = 3)

sum.city.armed <- wapo.data %>%
  group_by(statecity, armed_with) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

ggplot(sum.city.armed, aes(x = statecity, y = count, fill = factor(armed_with))) + 
  geom_bar (stat = "identity") +
  labs(x = "State-City", y = "Number of Police Shootings", 
       title = "Police Shootings by Victim Weapon Type", subtitle = "XXX", 
       fill = "Victims Weapon") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### example
# creates the summary table
sum.city.armed <- wapo.data %>%
  group_by(statecity, armed_with) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

# clean/filter select weapons based on DC's values
city.weapon <- subset(sum.city.armed, 
                           armed_with %in% c("replica", "gun", "unarmed",
                                             "knife","vehicle"))

# filter to select cities based on DC's overall count
wapo.city.select <- subset(city.weapon, statecity %in% 
                             c("DC-Washington","FL-Orlando","MI-Detroit",
                               "MD-Baltimore", 
                               "TN-Memphis","NC-Charlotte","OR-Portland"))

# graph
ggplot(wapo.city.select, aes(x = statecity, y = count, 
                             fill = factor(armed_with))) + 
  geom_bar (stat = "identity") +
  labs(x = "City", y = "Number of Police Shootings", 
       title = "Police Shootings by Victim Weapon Type", 
       subtitle = "xxx", fill = "Victims Weapon") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### maps
world <- ne_countries(scale = "medium", returnclass = "sf") # this builds a list of countries
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) # this cleans up the US states

# transparent points
ggplot(data = world) + 
  geom_sf(data = states, fill = NA) + 
  geom_point(data = wapo.data.map, aes(x = longitude, y = latitude), 
             size = 2, alpha = 0.025) + 
  coord_sf(xlim = c(-130, -65), ylim = c(25, 50), expand = FALSE)

ggplot(data = world) + 
  geom_sf(data = states, fill = NA) + 
  geom_point(data = wapo.data.map, aes(x = longitude, y = latitude), 
             size = 2, alpha = 0.025) + 
  coord_sf(xlim = c(-130, -65), ylim = c(25, 50), expand = FALSE) +
  facet_wrap(~race, nrow = 3)

leaflet(wapo.data.map) %>%
  addTiles() %>%
  addMarkers(lng = ~longitude, lat = ~latitude, 
             clusterOptions = markerClusterOptions())

# STEP 5
census_api_key("794f37c27092be60b131e4f207abcf950f227f38", 
               install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")

census.variables.2023 <- load_variables(2023, "acs5", cache = TRUE)

race.2023 <- get_acs(geography = "state", variables = c("B02008_001", "B02009_001", 
                                                        "B02010_001", "B02011_001", 
                                                        "B03001_003"), 
                     year = 2023)
race.2023$variable <- gsub("B02008_001", "White", race.2023$variable)
race.2023$variable <- gsub("B02009_001", "Black", race.2023$variable)
race.2023$variable <- gsub("B02010_001", "Native American", race.2023$variable)
race.2023$variable <- gsub("B02011_001", "Asian", race.2023$variable)
race.2023$variable <- gsub("B03001_003", "Hispanic", race.2023$variable)

race.total <- get_acs(geography = "us", variables = c("B02008_001", 
                                                      "B02009_001", 
                                                      "B02010_001", 
                                                      "B02011_001", 
                                                      "B03001_003"), 
                      year = 2023)
race.total$variable <- gsub("B02008_001", "White", race.total$variable)
race.total$variable <- gsub("B02009_001", "Black", race.total$variable)
race.total$variable <- gsub("B02010_001", "Native American", race.total$variable)
race.total$variable <- gsub("B02011_001", "Asian", race.total$variable)
race.total$variable <- gsub("B03001_003", "Hispanic", race.total$variable)

totalpop.2023 <- get_acs(geography = "state", variables = "B01003_001", 
                         year = 2023)

names(totalpop.2023) <- c("GEOID", "State", "Variable", "Population", "junk")
totalpop.2023$State.PCT <- round(totalpop.2023$Population/sum(totalpop.2023$Population)*100, 2)

temp.race.population <- race.2023 %>% 
  left_join(totalpop.2023, by = "GEOID")
race.population <- temp.race.population[c(1:4,8,10)]
names(race.population) <- c("GEOID", "State", "Race", "Population.Race", 
                            "Population.State", "State.PCT")
race.population$Race.PCT <- round(race.population$Population.Race/race.population$Population.State*100, 2)

race.total$PCT <- race.total$estimate/sum(totalpop.2023$Population)*100
names(race.total) <- c("GEOID", "Area", "Race", "Count", "moe", "Race.PCT")

sum.race$race <- gsub("W", "White", sum.race$race)
sum.race$race <- gsub("B", "Black", sum.race$race)
sum.race$race <- gsub("A", "Asian", sum.race$race)
sum.race$race <- gsub("N", "Native American", sum.race$race)
sum.race$race <- gsub("H", "Hispanic", sum.race$race)
sum.race$race <- gsub("O", "Other", sum.race$race)

names(sum.race) <- c("Race", "Count", "Shooting.PCT")

sum.state <- wapo.data %>%
  group_by(state) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

sum.state$State.Name <- state.name[match(sum.state$state,state.abb)]
sum.state$State.Name <- replace_na(sum.state$State.Name, "District of Columbia")
names(sum.state) <- c("oldstate", "Count", "Shooting.PCT", "State")

wapo.census.race <- sum.race %>% left_join(race.total, by = "Race")
wapo.census.race <- wapo.census.race[c(1:3,6,8)]
names(wapo.census.race) <- c("Race", "Shooting.Count", "Shooting.PCT",
                             "Population.Count", "Race.PCT")

wapo.census.state <- sum.state %>% left_join(totalpop.2023, by = "State")
wapo.census.state <- wapo.census.state[c(4,2,3,7,9)]

# new graphs
# Victim Race
ggplot(data = wapo.census.race, aes(x = Race)) +
  geom_bar(aes(y= Race.PCT), stat = "identity", position = "identity",
           alpha = 0.5, fill= 'lightblue', color= 'lightblue4') +
  geom_bar(aes(y= Shooting.PCT), stat = "identity", position = "identity",
           alpha = 0.5, fill = 'pink', color = 'red') +
  labs(title = "PCT of Population vs PCT of Police Shootings", 
       subtitle = "By Race", y = "Total PCT", "Race")

# Victim State
ggplot(data = wapo.census.state, aes(x= State)) +
  geom_bar(aes(y = State.PCT), stat = "identity", position = "identity",
           alpha = 0.5, fill = 'lightblue', color = 'lightblue4') +
  geom_bar(aes(y= Shooting.PCT), stat = "identity", position = "identity",
           alpha = 0.5, fill = 'pink', color = 'red') +
  labs(title = "PCT of Population vs PCT of Police Shootings",
       subtitle = "By State", y= "Total PCT", x = "State") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

sum.race.year <- wapo.data %>%
  group_by(race, year) %>%
  summarise(count = n()) %>%
  mutate(pct = round(count/sum(count)*100,2))

# By Race over time
ggplot() +
  geom_line(data = sum.race.year, aes(x = year, y= count, group = race,
                                      color = race)) +
  labs(title = "Number of Police Shootings", subtitle = "By Year and Race") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

