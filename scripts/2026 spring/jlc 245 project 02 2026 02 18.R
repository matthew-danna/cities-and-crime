# Step 1: libraries
library(tidyverse)

# Step 2: build a data table of the cities with just city names
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
    "San Jose, CA","New York City, NY","Memphis, TN"
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
    "Oklahoma City, OK","New York City, NY"
  ),
  transit.rank = 0:34
)

cities <- cities %>%
  left_join(transit_rank, by = "city.state")

