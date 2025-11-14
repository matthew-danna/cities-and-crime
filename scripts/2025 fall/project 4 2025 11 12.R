library(tidyverse)

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

# DRUG USAGE DATA
### read in drug usage CSV
drug.usage <- read.csv("C:/Users/danna/Downloads/drugusage.csv", stringsAsFactors = FALSE)
drug.usage <- drug.usage[c(1:5,8:15)]
drug.usage$total <- drug.usage$Marijuana + drug.usage$Cocaine +
  drug.usage$Heroin + drug.usage$Meth

drug.usage.similar <- drug.usage %>% filter(State %in% city.filter)

# DRUG OVERDOSE
drug.overdose <- read.csv("C:/Users/danna/Downloads/final.overdoses.csv",
                          stringsAsFactors = FALSE)
drug.overdose <- drug.overdose[c(2:6)]

names(drug.overdose) <- c("county", "county.code", "description",
                          "description.code", "overdoses")

drug.overdose$description.code <- as.character(drug.overdose$description.code)

drug.overdose.category <- drug.overdose %>%
  filter(!is.na(description.code) & description.code != "") %>%
  group_by(county, county.code, description.code) %>%
  summarise(overdoses = sum(overdoses, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = description.code,
    values_from = overdoses,
    values_fill = list(overdoses = 0)
  )


