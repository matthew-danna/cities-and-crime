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

# Step 6: Third data: health (not drugs)

# Step 7: Fourth data: Demographic/Socioeconomic

# Step 8: Visuals

### basic bar graph:
ggplot(cities, aes(x = reorder(city, firearm.rank), y = firearm.rank)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Firearm Deaths by City",
    x = "City",
    y = "Firearm Deaths"
  ) +
  theme_minimal()

### less basic bar graph
# Create custom categories
cities$category.firearm <- cut(
  cities$firearm.rank,
  breaks = c(-1,0, 6, 15, 25, 34),
  labels = c("DC (0)", "Very Low (1–6)", "Low (7–15)", "Meh (16-25)", "High (26-34)")
)

# Filter out DC and High cities
plot_data <- cities %>%
  filter(category.firearm %in% c("Very Low (1–6)", "Low (7–15)"))

# Plot
ggplot(plot_data, aes(x = reorder(city, -firearm.rank), y = firearm.rank, fill = category.firearm)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c(
    "Very Low (1–6)" = "blue",
    "Low (7–15)" = "orange"
  )) +
  labs(
    title = "Firearm Deaths by City (Excluding DC and High-Risk Cities)",
    x = "City",
    y = "Firearm Deaths",
    fill = "Category"
  ) +
  theme_minimal()

### scatterplot
ggplot(cities, aes(x = firearm.rank, y = transit.rank, label = city)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.5, size = 3) +
  labs(
    title = "Scatterplot of One Variable and Another Variable",
    x = "The first variable",
    y = "The other variable",
  ) +
  theme_minimal()

### scatterplot with highlights
# Add a highlight column
cities$highlight <- ifelse(cities$city == "Washington", "DC",
                           ifelse(cities$city == "Chicago", "Chicago", 
                                  "Other"))

ggplot(cities, aes(x = firearm.rank, y = transit.rank, label = city, 
                   color = highlight)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.5, size = 3, show.legend = FALSE) +
  scale_color_manual(values = c("DC" = "green", "Chicago" = "red", 
                                "Other" = "steelblue")) +
  labs(
    title = "Scatterplot of One Thing vs Another Thing",
    x = "First Thing",
    y = "Other Thing"
  ) +
  theme_minimal()

# another example 
cities$Similar <- ifelse(cities$city == "Washington", "DC",
                           ifelse(cities$city == "Baltimore", "Baltimore", 
                                  "Other"))

ggplot(cities, aes(x = firearm.rank, y = transit.rank, label = city, 
                   color = Similar)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.5, size = 3, show.legend = FALSE) +
  scale_color_manual(values = c("DC" = "green", "Baltimore" = "red", 
                                "Other" = "steelblue")) +
  labs(
    title = "Scatterplot of One Thing vs Another Thing",
    x = "Firearm Rank",
    y = "Transit Rank"
  ) +
  theme_minimal()



