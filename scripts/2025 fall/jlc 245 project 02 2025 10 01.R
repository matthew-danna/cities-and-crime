# packages needed
library(tidyverse)

# create the empty dataset
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



##### VISUALIZE
# graph 1
ggplot(data.rankings, aes(x = reorder(City, firearm.deaths), y = firearm.deaths)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Firearm Deaths by City",
    x = "City",
    y = "Firearm Deaths"
  ) +
  theme_minimal()

# graph 2
# Create custom categories
data.rankings$Category <- cut(
  data.rankings$firearm.deaths,
  breaks = c(-1, 0, 6, 10, 30),
  labels = c("DC (0)", "Very Similar (1–6)", "Kinda Similar (7–10)", "Not Similar (30)")
)

# Filter out DC and High cities
plot_data <- data.rankings %>%
  filter(Category %in% c("Very Similar (1–6)", "Kinda Similar (7–10)"))

# Plot
ggplot(plot_data, aes(x = reorder(City, -firearm.deaths), y = firearm.deaths, fill = Category)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c(
    "Very Similar (1–6)" = "blue",
    "Kinda Similar (7–10)" = "orange"
  )) +
  labs(
    title = "Firearm Deaths by City (Excluding DC and High-Risk Cities)",
    x = "City",
    y = "Firearm Deaths",
    fill = "Category"
  ) +
  theme_minimal()


#### 
# Add a highlight column
data.rankings$Highlight <- ifelse(data.rankings$City == "Washington DC", "DC",
                                  ifelse(data.rankings$City == "Chicago IL", "Chicago", "Other"))

ggplot(data.rankings, aes(x = firearm.deaths, y = diabetes.deaths, label = City, color = Highlight)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.5, size = 3) +
  scale_color_manual(values = c("DC" = "green", "Chicago" = "red", "Other" = "steelblue")) +
  labs(
    title = "Scatterplot of Firearm Deaths vs Diabetes Data",
    x = "Firearm Deaths",
    y = "Diabetes Data",
    color = "City"
  ) +
  theme_minimal()

# Add a highlight column
data.rankings$Highlight <- ifelse(data.rankings$City == "Washington DC", "DC",
                                  ifelse(data.rankings$City == "Kansas City MO", "Kansas City", "Other"))

ggplot(data.rankings, aes(x = diabetes.deaths, y = firearm.deaths, label = City, color = Highlight)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.5, size = 3) +
  scale_color_manual(values = c("DC" = "green", "Kansas City" = "purple", "Other" = "steelblue")) +
  labs(
    title = "Scatterplot of Diabetes x Firearms",
    x = "Diabetes Deaths",
    y = "Firearm Deaths",
    color = "City"
  ) +
  theme_minimal()
