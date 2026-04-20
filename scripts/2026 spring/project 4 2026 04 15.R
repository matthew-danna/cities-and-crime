#0: LIBRARIES
#install.packages('tidyverse')
#install.packages('devtools')
#devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)

library(scales)
library(ggradar)
library(tidyverse)

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
  "NY-New York City",    36005,   # Bronx  
  "NY-New York City",    36047,   # Kings (Brooklyn)  
  "NY-New York City",    36061,   # New York (Manhattan)  
  "NY-New York City",    36081,   # Queens  
  "NY-New York City",    36085,   # Richmond (Staten Island)  
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

# Step 3 - VISUALS!
### radar chart
radar.data <- drugs %>%
  select(statecity,
         use.marijuana,
         use.cocaine,
         use.heroin,
         use.meth,
         od.opioids,
         od.cocaine,
         od.stimulants,
         od.painkillers,
         od.sedatives,
         od.psychoactive,
         od.antiepileptic,
         od.tobacco) %>%
  # Scale each variable 0–1 across all cities
  mutate(across(-statecity, ~ rescale(.x, to = c(0, 1), na.rm = TRUE)))

### make a function a radar plot for one city
plot_city_radar <- function(city_name) {
  drug.data <- radar.data %>% filter(statecity == city_name)
  ggradar(drug.data,
          grid.min = 0,
          grid.mid = 0.5,
          grid.max = 1,
          group.line.width = 1,
          group.point.size = 2,
          background.circle.colour = "grey90",
          plot.title = city_name)
}

### choose a city, get a plot
# some examples
plot_city_radar("IL-Chicago")
plot_city_radar("DC-Washington")

