library(tidyverse)

# build a data table of the cities

cities <- data.frame(
  city = c(
    "Austin", "Baltimore", "Boston", "Charlotte", "Chicago",
    "Cleveland", "Columbus", "Dallas", "Denver", "Detroit",
    "El Paso", "Houston", "Indianapolis", "Kansas City",
    "Las Vegas", "Long Beach", "Los Angeles", "Louisville",
    "Memphis", "Milwaukee", "Minneapolis", "Nashville",
    "New York City", "Oakland", "Oklahoma City", "Philadelphia",
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

# Create city_state label if you don't already have it
cities <- cities %>%
  mutate(city_state = paste(city, state, sep = ", "))

# Define high cities
high_cities <- c(
  "Memphis, TN",
  "Detroit, MI",
  "Cleveland, OH",
  "Milwaukee, WI",
  "Baltimore, MD"
)

low_cities <- c(
  "Oakland, CA",
  "San Antonio, TX",
  "Denver, CO",
  "Phoenix, AZ",
  "Dallas, TX",
  "Columbus, OH",
  "Las Vegas, NV",
  "Portland, OR",
  "Oklahoma City, OK",
  "Charlotte, NC",
  "Austin, TX",
  "Minneapolis, MN",
  "El Paso, TX",
  "Los Angeles, CA",
  "Long Beach, CA",
  "Seattle, WA",
  "San Francisco, CA",
  "San Diego, CA",
  "Boston, MA",
  "San Jose, CA",
  "New York City, NY"
)

cities <- cities %>%
  mutate(
    crime.firearms = case_when(
      city_state %in% high_cities ~ "high",
      city_state %in% low_cities  ~ "low",
      TRUE                        ~ "medium"
    )
  )
