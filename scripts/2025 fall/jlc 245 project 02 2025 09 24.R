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

# add the first metric (Firearm Death Rate)
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
  labels = c("DC (0)", "Very Low (1–6)", "Low (7–10)", "High (30)")
)

# Filter out DC and High cities
plot_data <- data.rankings %>%
  filter(Category %in% c("Very Low (1–6)", "Low (7–10)"))

# Plot
ggplot(plot_data, aes(x = reorder(City, -firearm.deaths), y = firearm.deaths, fill = Category)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c(
    "Very Low (1–6)" = "blue",
    "Low (7–10)" = "orange"
  )) +
  labs(
    title = "Firearm Deaths by City (Excluding DC and High-Risk Cities)",
    x = "City",
    y = "Firearm Deaths",
    fill = "Category"
  ) +
  theme_minimal()

#### 
####  
####
# Create a vector of random numbers 1–34
random_values <- sample(1:34, size = nrow(data.rankings) - 1, replace = FALSE)

# Assign fake.data
data.rankings$fake.data <- NA
data.rankings$fake.data[data.rankings$City == "Washington DC"] <- 0
data.rankings$fake.data[data.rankings$City != "Washington DC"] <- random_values

#### 
# Add a highlight column
data.rankings$Highlight <- ifelse(data.rankings$City == "Washington DC", "DC",
                                  ifelse(data.rankings$City == "Chicago IL", "Chicago", "Other"))

ggplot(data.rankings, aes(x = firearm.deaths, y = fake.data, label = City, color = Highlight)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.5, size = 3) +
  scale_color_manual(values = c("DC" = "green", "Chicago" = "red", "Other" = "steelblue")) +
  labs(
    title = "Scatterplot of Firearm Deaths vs Fake Data",
    x = "Firearm Deaths",
    y = "Fake Data",
    color = "City"
  ) +
  theme_minimal()
