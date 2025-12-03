#install.packages('cluster')
#install.packages('ggpubr')
#install.packages('ggrepel')
library(cluster)
library(tidyverse)
library(ggpubr)
library(ggrepel)

# DATA!

setwd("C:/Users/danna/Downloads") # for windows
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
names(data.rankings) <- c("city.state", "proj2.firearm", "proj2.diabetes")

### join project 2 and project 3 datasets
final.data <- data.rankings %>%
  left_join(rankings.combined)

final.data <- final.data[c(1:11)]

### the master list of cities
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

# Step 2: DRUG USAGE DATA
### read in American Addiction Center data
drug.usage <- read.csv("https://raw.githubusercontent.com/matthew-danna/cities-and-crime/refs/heads/main/data/american_addicition_centers.csv", 
                       stringsAsFactors = FALSE)

### select columns
drug.usage <- drug.usage[c(1:5,8:15)]

### calculate totals
drug.usage$total <- drug.usage$Marijuana + drug.usage$Cocaine +
  drug.usage$Heroin + drug.usage$Meth

### filter to master city list
drug.usage.similar <- drug.usage %>% filter(State %in% city.filter)

### rename columns
names(drug.usage.similar) <- c("city.state",  "use.marijuana", "use.cocaine", "use.heroin", "use.meth", 
                               "use.diff.marijuana", "use.diff.cocaine", "use.diff.heroin", "use.diff.meth", 
                               "use.rank.marijuana", "use.rank.cocaine", "use.rank.heroin", "use.rank.meth", "use.total")

### restructure table
drug.usage.similar <- drug.usage.similar[c(1,14,2,6,10,3,7,11,4,8,12,5,9,13)]

# Step 3: DRUG OVERDOSE DATA
### read in CDC data
drug.overdose <- read.csv("https://raw.githubusercontent.com/matthew-danna/cities-and-crime/refs/heads/main/data/final.overdoses.csv",
                          stringsAsFactors = FALSE)

### select columns
drug.overdose <- drug.overdose[c(2:6)]

### rename columns
names(drug.overdose) <- c("county", "county.code", "description",
                          "description.code", "overdoses")

### format field as a character to avoid math
drug.overdose$description.code <- as.character(drug.overdose$description.code)

### restructure table so each unique drug value is a column, not a row
drug.overdose.category <- drug.overdose %>%
  filter(!is.na(description.code) & description.code != "") %>%
  group_by(county, county.code, description.code) %>%
  summarise(overdoses = sum(overdoses, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = description.code,
    values_from = overdoses,
    values_fill = list(overdoses = 0)
  )

### build a look-up table to maps counties to cities
### this ain't perfect but good enough
city.county <- tribble(
  ~city,                 ~county.code,
  "Austin TX",           48453,   # Travis County  [oai_citation:0‡U.S. Office of Personnel Management](https://www.opm.gov/policy-data-oversight/pay-leave/salaries-wages/2024/locality-pay-area-definitions/?utm_source=chatgpt.com)  
  "Baltimore MD",        24510,   # Baltimore City (independent)  [oai_citation:1‡HMDB](https://www.hmdb.org/countyoverlay/countyseatlist.asp?utm_source=chatgpt.com)  
  "Boston MA",           25025,   # Suffolk County  
  "Charlotte NC",        37119,   # Mecklenburg County  
  "Chicago IL",          17031,   # Cook County  [oai_citation:2‡University Library](https://www.library.illinois.edu/max/collections/air-photos-new/air-photo-indexes/fips_codes_il_counties/?utm_source=chatgpt.com)  
  "Cleveland OH",        39035,   # Cuyahoga County  
  "Columbus OH",         39049,   # Franklin County  
  "Dallas TX",           48113,   # Dallas County (major)  
  "Denver CO",           08031,   # Denver County  
  "Detroit MI",          26163,   # Wayne County  
  "El Paso TX",          48141,   # El Paso County  
  "Houston TX",          48201,   # Harris County  [oai_citation:3‡Wikipedia](https://en.wikipedia.org/wiki/Houston?utm_source=chatgpt.com)  
  "Houston TX",          48157,   # Fort Bend County (part)  [oai_citation:4‡U.S. Office of Personnel Management](https://www.opm.gov/policy-data-oversight/pay-leave/salaries-wages/2011/locality-pay-area-definitions/?utm_source=chatgpt.com)  
  "Houston TX",          48339,   # Montgomery County (part)  [oai_citation:5‡U.S. Office of Personnel Management](https://www.opm.gov/policy-data-oversight/pay-leave/salaries-wages/2011/locality-pay-area-definitions/?utm_source=chatgpt.com)  
  "Indianapolis IN",     18097,   # Marion County  
  "Kansas City MO",      29095,   # Jackson County  [oai_citation:6‡Wikipedia](https://en.wikipedia.org/wiki/List_of_counties_in_Missouri?utm_source=chatgpt.com)  
  "Kansas City MO",      29047,   # Clay County  [oai_citation:7‡mcdc.missouri.edu](https://mcdc.missouri.edu/geography/reference/mocnty2uauc.html?utm_source=chatgpt.com)  
  "Kansas City MO",      29037,   # Cass County  [oai_citation:8‡mcdc.missouri.edu](https://mcdc.missouri.edu/geography/reference/mocnty2uauc.html?utm_source=chatgpt.com)  
  "Kansas City MO",      29165,   # Platte County  [oai_citation:9‡Wikipedia](https://en.wikipedia.org/wiki/Platte_County%2C_Missouri?utm_source=chatgpt.com)  
  "Las Vegas NV",        32003,   # Clark County  
  "Long Beach CA",       06037,   # Los Angeles County  
  "Los Angeles CA",      06037,   # Los Angeles County  
  "Louisville KY",       21111,   # Jefferson County  
  "Memphis TN",          47157,   # Shelby County  
  "Milwaukee WI",        55079,   # Milwaukee County  
  "Minneapolis MN",      27053,   # Hennepin County  
  "Nashville TN",        47037,   # Davidson County  
  "New York City NY",    36005,   # Bronx  
  "New York City NY",    36047,   # Kings (Brooklyn)  
  "New York City NY",    36061,   # New York (Manhattan)  
  "New York City NY",    36081,   # Queens  
  "New York City NY",    36085,   # Richmond (Staten Island)  
  "Oakland CA",          06001,   # Alameda County  
  "Oklahoma City OK",    40109,   # Oklahoma County  
  "Oklahoma City OK",    40017,   # Canadian County  
  "Oklahoma City OK",    40027,   # Cleveland County  
  "Oklahoma City OK",    40125,   # Pottawatomie County  
  "Philadelphia PA",     42101,   # Philadelphia County  
  "Phoenix AZ",          04013,   # Maricopa County  
  "Phoenix AZ",          04021,   # (Note: some parts may extend into Pinal County)  
  "Portland OR",         41051,   # Multnomah County  
  "San Antonio TX",      48029,   # Bexar County  
  "San Diego CA",        06073,   # San Diego County  
  "San Francisco CA",    06075,   # San Francisco County (city-county)  [oai_citation:10‡HMDB](https://www.hmdb.org/countyoverlay/countyseatlist.asp?utm_source=chatgpt.com)  
  "San Jose CA",         06085,   # Santa Clara County  
  "Seattle WA",          53033,   # King County  
  "Tucson AZ",           04019,   # Pima County  
  "Washington DC",       11001    # District of Columbia (equivalent to county)  [oai_citation:11‡HMDB](https://www.hmdb.org/countyoverlay/countyseatlist.asp?utm_source=chatgpt.com)  
)

### append city name to relevant counties
drug.od <- drug.overdose.category %>%
  right_join(city.county)

### rename columns
### this is TEDIOUS and not perfect - but it transforms OD codes to usable categories
names(drug.od) <- c("county", "county.code", 
                    "tobacco", "undetermined1", "psychoactive1", "antiepileptic1", "hallucinogens1", "undetermined2",
                    "multiple1", "hallucinogens2", "opioids1", "opioids2", "sedatives1", "cocaine1", "stimulants1", 
                    "stimulants2", "solvents", "antiepileptic2", "antiepileptic3", "hallucinogens3", "multiple2", 
                    "cannabinoids1", "cocaine2", "multiple3", "assault" ,"psychoactive2", "opioids3", "cocaine3", 
                    "analgesics1", "cannabinoids2", "sedatives2", "analgesics2", "psychoactive3", "stimulants3", 
                    "opioids4", "analgesics3",
                    "city")

### calculate totals across similar columns
drug.od$undetermined <- drug.od$undetermined1 + drug.od$undetermined2
drug.od$psychoactive <- drug.od$psychoactive1 + drug.od$psychoactive2 + drug.od$psychoactive3
drug.od$antiepileptic <- drug.od$antiepileptic1 + drug.od$antiepileptic2 + drug.od$antiepileptic3
drug.od$hallucinogens <- drug.od$hallucinogens1 + drug.od$hallucinogens2 + drug.od$hallucinogens3
drug.od$multiple <- drug.od$multiple1 + drug.od$multiple2 + drug.od$multiple3
drug.od$opioids <- drug.od$opioids1 + drug.od$opioids2 + drug.od$opioids3 + drug.od$opioids4
drug.od$sedatives <- drug.od$sedatives1 + drug.od$sedatives2
drug.od$cocaine <- drug.od$cocaine1 + drug.od$cocaine2 + drug.od$cocaine3
drug.od$stimulants <- drug.od$stimulants1 + drug.od$stimulants2 + drug.od$stimulants3
drug.od$cannabinoids <- drug.od$cannabinoids1 + drug.od$cannabinoids2
drug.od$painkillers <- drug.od$analgesics1 + drug.od$analgesics2 + drug.od$analgesics3

overdoses <- drug.od[c(37,1,3,17,25,38:48)]

names(overdoses) <- c("city.state", "county", "od.tobacco", "od.solvents", "od.assault", "od.undetermined", 
                      "od.psychoactive", "od.antiepileptic", "od.hallucinogens", "od.multiple", "od.opioids",
                      "od.sedatives", "od.cocaine", "od.stimulants", "od.cannabinoids", "od.painkillers")

### collapse all county rows into one row per city
city.overdoses <- overdoses %>%
  group_by(city.state) %>%
  summarise(across(
    .cols = where(is.numeric),
    .fns  = ~ sum(.x, na.rm = TRUE)
  )) %>%
  ungroup()

### rank
city.overdoses$od.diff.tobacco <- abs(city.overdoses$od.tobacco - city.overdoses$od.tobacco[35])
city.overdoses <- city.overdoses[order(city.overdoses$od.diff.tobacco),]
city.overdoses$od.rank.tobacco <- seq.int(nrow(city.overdoses)) -1

city.overdoses$od.diff.opioids <- abs(city.overdoses$od.opioids - city.overdoses$od.opioids[1])
city.overdoses <- city.overdoses[order(city.overdoses$od.diff.opioids),]
city.overdoses$od.rank.opioids <- seq.int(nrow(city.overdoses)) -1

# copy/paste above 3 lines 6 more times for additional drugs
# recommend you focus on these: 
# opioids, cocaine, stimulants, tobacco, psychoactive, antiepileptic, hallucinogens, & painkillers

### Step 4: JOIN
### combine drug datas
drugs <- city.overdoses %>%
  left_join(drug.usage.similar)

### THIS IS THE NEW CODE TO COMBINE ALL THE DATAS
data.final <- final.data %>%
  left_join(drugs)

### this is an example - choose your own columns of interest!
data.final <- data.final[c(1:3,5:11,27,29,33,36,39,42)]

#############
# CALCULATE MDS
##### please update the column numbering at the end of this line to match the table above!
mds.city <- cmdscale(dist(data.final[, 2:16]))

# EMPTY PLOT
plot(mds.city[, 1], mds.city[, 2], 
     type = "n", xlab = "MDS Dimension 1", 
     ylab = "MDS Dimension 2")

# ADD POINTS
points(mds.city[, 1], mds.city[, 2], 
       pch = 21, bg = "lightblue")
text(mds.city[, 1], mds.city[, 2], 
     labels = data.final$city.state, 
     pos = 3, cex = 0.8)

# CALCULATE K MEANS CLUSTERING
clusters <- kmeans(mds.city, centers = 8)$cluster

# ADD CLUSTERS TO THE PLOT
points(mds.city[, 1], mds.city[, 2], 
       pch = 21, bg = clusters, cex = 1.2)

# UPDATE DATA TABLE
mds.df <- as.data.frame(mds.city)
mds.df$groups <- as.factor(clusters)
mds.df$city <- data.final$city.state  # Add species information

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

