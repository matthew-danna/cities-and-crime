# Step 0: PACKAGES
### run once
devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)

### run everytime
library(scales)
library(ggradar)
library(tidyverse)

# Step 1: DATA PREP
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

# Step 5: VISUALS
### select and scale all relevant columns
radar.data <- drugs %>%
  select(city.state,
         use.marijuana,
         use.cocaine,
         use.heroin,
         use.meth,
         od.opioids,
         od.cocaine,
         od.stimulants,
         od.painkillers) %>%
  # Scale each variable 0–1 across all cities
  mutate(across(-city.state, ~ rescale(.x, to = c(0, 1), na.rm = TRUE)))

### make a function a radar plot for one city
plot_city_radar <- function(city_name) {
  df <- radar.data %>% filter(city.state == city_name)
  ggradar(df,
          grid.min = 0,
          grid.mid = 0.5,
          grid.max = 1,
          group.line.width = 1,
          group.point.size = 2,
          background.circle.colour = "grey90",
          plot.title = city_name)
}

### choose a city, get a plot
plot_city_radar("Washington DC")



