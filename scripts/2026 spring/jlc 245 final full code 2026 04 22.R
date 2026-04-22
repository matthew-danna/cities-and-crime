##### PROJECT 2
# Step 1: libraries
library(tidyverse)

# MAKE SURE TO CHANGE ANY 'New York City' REFERENCES TO JUST 'New York'

# Step 2: build a data table of the cities with just city names
cities <- data.frame(
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

# Step 3: Create city.state label
cities <- cities %>%
  mutate(city.state = paste(city, state, sep = ", "))

# Step 4: First dataset: firearm deaths
firearm_rank <- data.frame(
  city.state = c(
    "Washington, DC","Tucson, AZ","Houston, TX","Chicago, IL",
    "Louisville, KY","Philadelphia, PA","Nashville, TN",
    "Indianapolis, IN","Oakland, CA","San Antonio, TX",
    "Denver, CO","Phoenix, AZ","Dallas, TX","Columbus, OH",
    "Las Vegas, NV","Kansas City, MO","Portland, OR",
    "Oklahoma City, OK","Charlotte, NC","Baltimore, MD",
    "Milwaukee, WI","Austin, TX","Minneapolis, MN",
    "El Paso, TX","Cleveland, OH","Los Angeles, CA",
    "Long Beach, CA","Seattle, WA","Detroit, MI",
    "San Francisco, CA","San Diego, CA","Boston, MA",
    "San Jose, CA","New York, NY","Memphis, TN"
  ),
  firearm.rank = 0:34
)

cities <- cities %>%
  left_join(firearm_rank, by = "city.state")

# Step 5: Second data: public transportation
transit_rank <- data.frame(
  city.state = c(
    "Washington, DC","San Francisco, CA","Boston, MA",
    "Chicago, IL","Philadelphia, PA","Oakland, CA",
    "Seattle, WA","Baltimore, MD","Minneapolis, MN",
    "Portland, OR","Cleveland, OH","Los Angeles, CA",
    "Detroit, MI","Milwaukee, WI","Denver, CO",
    "Long Beach, CA","Houston, TX","San Diego, CA",
    "San Jose, CA","Las Vegas, NV","Tucson, AZ",
    "Dallas, TX","Charlotte, NC","Louisville, KY",
    "Columbus, OH","San Antonio, TX","Austin, TX",
    "Phoenix, AZ","Kansas City, MO","Nashville, TN",
    "Indianapolis, IN","El Paso, TX","Memphis, TN",
    "Oklahoma City, OK","New York, NY"
  ),
  transit.rank = 0:34
)

cities <- cities %>%
  left_join(transit_rank, by = "city.state")

# Step 6: Third data: health (not drugs)

# Step 7: Fourth data: Demographic/Socioeconomic

##### PROJECT 3
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

##### PROJECT 4
#1: READ IN DRUG USAGE
drug.usage <- read.csv("https://raw.githubusercontent.com/matthew-danna/cities-and-crime/refs/heads/main/data/aac%20drug%20usage%202026.csv", 
                       stringsAsFactors = FALSE)

#2: CREATE STATECITY VARIABLE
drug.usage$statecity <- paste(drug.usage$state, drug.usage$city, sep = "-")

#3: RENAME COLUMNS
names(drug.usage) <- c("city", "state", "use.marijuana", "use.cocaine", 
                       "use.heroin", "use.meth", "use.diff.marijuana", 
                       "use.rank.marijuana", "use.diff.cocaine",  
                       "use.rank.cocaine","use.diff.heroin", 
                       "use.rank.heroin", "use.diff.meth", 
                       "use.rank.meth", "statecity")

#4: READ IN DRUG OVERDOSES
drug.overdose <- read.csv("https://raw.githubusercontent.com/matthew-danna/cities-and-crime/refs/heads/main/data/cdc%20data%202026.csv", 
                          stringsAsFactors = FALSE)

#5: REMOVE JUNK COLUMNS
drug.overdose <- drug.overdose[c(2:6)]

#6: RENAME COLUMNS 
names(drug.overdose) <- c("county", "county.code", "description", 
                          "description.code", "overdoses")

#7: FORMAT COLUMN AS TEXT
drug.overdose$description.code <- as.character(drug.overdose$description.code)

# MAKE SURE TO CHANGE ANY 'New York City' REFERENCES TO JUST 'New York'
#8: MAKE A COUNTY-TO-CITY REFERENCE TABLE
city.county <- tribble(
  ~city,                 ~county.code,
  "TX-Austin",           48453,   # Travis County  [oai_citation:0‡U.S. Office of Personnel Management](https://www.opm.gov/policy-data-oversight/pay-leave/salaries-wages/2024/locality-pay-area-definitions/?utm_source=chatgpt.com)  
  "MD-Baltimore",        24510,   # Baltimore City (independent)  [oai_citation:1‡HMDB](https://www.hmdb.org/countyoverlay/countyseatlist.asp?utm_source=chatgpt.com)  
  "MA-Boston",           25025,   # Suffolk County  
  "NC-Charlotte",        37119,   # Mecklenburg County  
  "IL-Chicago",          17031,   # Cook County  [oai_citation:2‡University Library](https://www.library.illinois.edu/max/collections/air-photos-new/air-photo-indexes/fips_codes_il_counties/?utm_source=chatgpt.com)  
  "OH-Cleveland",        39035,   # Cuyahoga County  
  "OH-Columbus",         39049,   # Franklin County  
  "TX-Dallas",           48113,   # Dallas County (major)  
  "CO-Denver",           08031,   # Denver County  
  "MI-Detroit",          26163,   # Wayne County  
  "TX-El Paso",          48141,   # El Paso County  
  "TX-Houston",          48201,   # Harris County  [oai_citation:3‡Wikipedia](https://en.wikipedia.org/wiki/Houston?utm_source=chatgpt.com)  
  "TX-Houston",          48157,   # Fort Bend County (part)  [oai_citation:4‡U.S. Office of Personnel Management](https://www.opm.gov/policy-data-oversight/pay-leave/salaries-wages/2011/locality-pay-area-definitions/?utm_source=chatgpt.com)  
  "TX-Houston",          48339,   # Montgomery County (part)  [oai_citation:5‡U.S. Office of Personnel Management](https://www.opm.gov/policy-data-oversight/pay-leave/salaries-wages/2011/locality-pay-area-definitions/?utm_source=chatgpt.com)  
  "IN-Indianapolis",     18097,   # Marion County  
  "MO-Kansas City",      29095,   # Jackson County  [oai_citation:6‡Wikipedia](https://en.wikipedia.org/wiki/List_of_counties_in_Missouri?utm_source=chatgpt.com)  
  "MO-Kansas City",      29047,   # Clay County  [oai_citation:7‡mcdc.missouri.edu](https://mcdc.missouri.edu/geography/reference/mocnty2uauc.html?utm_source=chatgpt.com)  
  "MO-Kansas City",      29037,   # Cass County  [oai_citation:8‡mcdc.missouri.edu](https://mcdc.missouri.edu/geography/reference/mocnty2uauc.html?utm_source=chatgpt.com)  
  "MO-Kansas City",      29165,   # Platte County  [oai_citation:9‡Wikipedia](https://en.wikipedia.org/wiki/Platte_County%2C_Missouri?utm_source=chatgpt.com)  
  "NV-Las Vegas",        32003,   # Clark County  
  "CA-Long Beach",       06037,   # Los Angeles County  
  "CA-Los Angeles",      06037,   # Los Angeles County  
  "KY-Louisville",       21111,   # Jefferson County  
  "TN-Memphis",          47157,   # Shelby County  
  "WI-Milwaukee",        55079,   # Milwaukee County  
  "MN-Minneapolis",      27053,   # Hennepin County  
  "TN-Nashville",        47037,   # Davidson County  
  "NY-New York",    36005,   # Bronx  
  "NY-New York",    36047,   # Kings (Brooklyn)  
  "NY-New York",    36061,   # New York (Manhattan)  
  "NY-New York",    36081,   # Queens  
  "NY-New York",    36085,   # Richmond (Staten Island)  
  "CA-Oakland",          06001,   # Alameda County  
  "OK-Oklahoma City",    40109,   # Oklahoma County  
  "OK-Oklahoma City",    40017,   # Canadian County  
  "OK-Oklahoma City",    40027,   # Cleveland County  
  "OK-Oklahoma City",    40125,   # Pottawatomie County  
  "PA-Philadelphia",     42101,   # Philadelphia County  
  "AZ-Phoenix",          04013,   # Maricopa County  
  "AZ-Phoenix",          04021,   # (Note: some parts may extend into Pinal County)  
  "OR-Portland",         41051,   # Multnomah County  
  "TX-San Antonio",      48029,   # Bexar County  
  "CA-San Diego",        06073,   # San Diego County  
  "CA-San Francisco",    06075,   # San Francisco County (city-county)  [oai_citation:10‡HMDB](https://www.hmdb.org/countyoverlay/countyseatlist.asp?utm_source=chatgpt.com)  
  "CA-San Jose",         06085,   # Santa Clara County  
  "WA-Seattle",          53033,   # King County  
  "AZ-Tucson",           04019,   # Pima County  
  "DC-Washington",       11001    # District of Columbia (equivalent to county)  [oai_citation:11‡HMDB](https://www.hmdb.org/countyoverlay/countyseatlist.asp?utm_source=chatgpt.com)  
)

#9: CORRELATE COUNTIES TO CITIES
drug.overdose.city <- drug.overdose %>%
  right_join(city.county, by = 'county.code')

#10: TRANSPOSE ROWS TO COLUMNS
drug.overdose.category <- drug.overdose.city %>%
  filter(!is.na(description.code) & description.code != "") %>%
  group_by(county, county.code, description.code, city) %>%
  summarise(overdoses = sum(overdoses, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = description.code,
    values_from = overdoses,
    values_fill = list(overdoses = 0)
  )

#11: RENAME COLUMNS TO ACTUAL DRUGS
names(drug.overdose.category) <- c("county", "county.code", "city", "opioids1", "cocaine1", "cocaine2", 
                                   "stimulants1", "stimulants2", "tobacco", "psychoactive1", "psychoactive2", 
                                   "antiepileptic1", "hallucinogens1", "unspecified1", "antiepileptic2", 
                                   "hallucinogens2", "unspecified2", "opioids2", "sedatives", "analgesics1", 
                                   "assault", "antiepileptic3", "hallucinogens3", "unspecified2", "analgesics2", 
                                   "psychoactive3", "opioids3", "cannabinoids", "solvents", "stimulants3")

#12: ADD COMMON COLUMNS TOGETHER
drug.overdose.category$unspecified <- drug.overdose.category$unspecified1 + drug.overdose.category$unspecified2
drug.overdose.category$psychoactive <- drug.overdose.category$psychoactive1 + drug.overdose.category$psychoactive2 + drug.overdose.category$psychoactive3
drug.overdose.category$antiepileptic <- drug.overdose.category$antiepileptic1 + drug.overdose.category$antiepileptic2 + drug.overdose.category$antiepileptic3
drug.overdose.category$hallucinogens <- drug.overdose.category$hallucinogens1 + drug.overdose.category$hallucinogens2 + drug.overdose.category$hallucinogens3
drug.overdose.category$opioids <- drug.overdose.category$opioids1 + drug.overdose.category$opioids2 + drug.overdose.category$opioids3
drug.overdose.category$cocaine <- drug.overdose.category$cocaine1 + drug.overdose.category$cocaine2
drug.overdose.category$stimulants <- drug.overdose.category$stimulants1 + drug.overdose.category$stimulants2 + drug.overdose.category$stimulants3
drug.overdose.category$painkillers <- drug.overdose.category$analgesics1 + drug.overdose.category$analgesics2

#13: REMOVE JUNK COLUMNS
overdoses <- drug.overdose.category[c(3,1,2,9,19,21,28,29,31:38)]

#14: RENAME COLUMNS
names(overdoses) <- c("statecity", "county", "county.code", "od.tobacco", "od.sedatives", "od.assault", 
                      "od.cannabinoids", "od.solvents", "od.unspecified", "od.psychoactive", "od.antiepileptic", 
                      "od.hallucinogens", "od.opioids", "od.cocaine", "od.stimulants", "od.painkillers")

#15: COLLAPSE ONE ROW PER CITY
city.overdoses <- overdoses %>%
  group_by(statecity) %>%
  summarise(across(
    .cols = where(is.numeric),
    .fns  = ~ sum(.x, na.rm = TRUE)
  )) %>%
  ungroup()

#16: JOIN OVERDOSES AND USAGE
drugs <- city.overdoses %>%
  left_join(drug.usage)

##### new stuff
#1: rename tables
final.project2 <- cities
final.project3 <- project3.combined
final.project4 <- drugs

#2: statecity
final.project2$statecity <- paste(final.project2$state,
                                  final.project2$city,
                                  sep = "-")

#3: join
final <- list(
  final.project2,
  final.project3,
  final.project4
) %>% reduce(full_join, by = "statecity")

#4: clean
colnames(final)
##### ADJUST THESE COLUMNS AS NECESSARY FOR YOUR DATA
final <- final[c(6,4,5,9,11,13,15,17,19,22:34,37:40)]

##### THE EXAMPLE CODE IS NOW PASTED BELOW
# CALCULATE MDS
### update the table name and the column numbers accordingly!
mds.city <- cmdscale(dist(final[, 2:26]))

# EMPTY PLOT
plot(mds.city[, 1], mds.city[, 2], 
     type = "n", xlab = "MDS Dimension 1", 
     ylab = "MDS Dimension 2")

# ADD POINTS
points(mds.city[, 1], mds.city[, 2], 
       pch = 21, bg = "lightblue")
text(mds.city[, 1], mds.city[, 2], 
     labels = final$statecity, 
     pos = 3, cex = 0.8)

# CALCULATE K MEANS CLUSTERING
### UPDATE THE NUMBER FOR CENTERS ACCORDINGLY!!
clusters <- kmeans(mds.city, centers = 8)$cluster

# ADD CLUSTERS TO THE PLOT
points(mds.city[, 1], mds.city[, 2], 
       pch = 21, bg = clusters, cex = 1.2)

# UPDATE DATA TABLE
mds.df <- as.data.frame(mds.city)
mds.df$groups <- as.factor(clusters)
mds.df$city <- final$statecity

# UPDATED PLOT WITH CLUSTERS
ggscatter(mds.df, x = "V1", y = "V2",
          color = "groups",
          palette = "jco",
          size = 3,
          ellipse = TRUE,
          ellipse.type = "convex",
          title = "K-means Clustering of MDS Cities Data",
          xlab = "MDS Dimension 1",
          ylab = "MDS Dimension 2") +
  geom_text_repel(aes(label = city), box.padding = 0.5)



