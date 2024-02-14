# Step 0
#install.packages('tidyverse')
#install.packages('ggrepel')
install.packages('googlesheets4') # new for this project
install.packages('ggalt') # new for this project
install.packages('tidyquant') # new for this project

library(tidyverse)
library(ggalt)
library(tidyquant)
library(ggrepel)
library(googlesheets4)

# Step 1
gs4_deauth()

# 2022-2023 murder data
temp.murder.new <- read_sheet("https://docs.google.com/spreadsheets/d/18P1K2qnKQuf31GArQ4aVQrDR-TbjUpdUpAtzlh8OZMA/edit?usp=sharing", 
                              sheet = "Sheet1")
# 2019-2020 murder data
temp.murder.old <- read_sheet("https://docs.google.com/spreadsheets/d/1Z9b5mIwztAwmEHJW7Q5DHMjS14-Rs7XIXOt33Al_rDw/edit#gid=1757262194", 
                              sheet = "Just Murders")
# 2019-2020 violent and property crime
temp.city.crime <- read_sheet("https://docs.google.com/spreadsheets/d/1Z9b5mIwztAwmEHJW7Q5DHMjS14-Rs7XIXOt33Al_rDw/edit#gid=1757262194", 
                              sheet = "All Crime")

# Step 2
# old murder
murder.old <- temp.murder.old[c(1:3,7)]
names(murder.old) <- c("city", "year2020", "year2019", "party")
murder.old <- subset(murder.old, !is.na(murder.old$year2020))
murder.old$city <- gsub("Miami - COUNTY NOT CITY", "Miami", murder.old$city)

# new murder
murder.new <- temp.murder.new[c(1:3)]
names(murder.new) <- c("city", "year2023", "year2022")
murder.new$city <- gsub("Miami - COUNTY", "Miami", murder.new$city)
murder.new$year2023 <- as.numeric(murder.new$year2023)
murder.new$year2022 <- as.numeric(murder.new$year2022)

# city crime
city.crime <- temp.city.crime[c(1,4:9)]
names(city.crime) <- c("city", "violent2020", "violent2019", "prop2020", "prop2019", "total2020", "total2019")
city.crime <- city.crime[-c(27,28),]
city.crime$violent2020 <- round(as.numeric(city.crime$violent2020), 0)
city.crime$prop2020 <- round(as.numeric(city.crime$prop2020), 0)
city.crime$total2020 <- round(as.numeric(city.crime$total2020), 0)

# Step 3
# murder old
murder.old$pct <- round(((murder.old$year2020-murder.old$year2019)/murder.old$year2019),2)
murder.old$dc2020 <- murder.old$year2020 - 197
murder.old$dc2019 <- murder.old$year2019 - 165
murder.old$dcpct <- murder.old$pct - 0.19

# murder new
murder.new$pct <- round(((murder.new$year2023-murder.new$year2022)/murder.new$year2022),2)
murder.new$dc2023 <- murder.new$year2023 - 274
murder.new$dc2022 <- murder.new$year2022 - 201
murder.new$dcpct <- murder.new$pct - 0.36

# city crime
city.crime$pct.violent <- round(((city.crime$violent2020-city.crime$violent2019)/city.crime$violent2019),2)
city.crime$pct.property <- round(((city.crime$prop2020-city.crime$prop2019)/city.crime$prop2019),2)
city.crime$pct.total <- round(((city.crime$total2020-city.crime$total2019)/city.crime$total2019),2)

# Step 4
# scatterplots
ggplot(murder.old) + 
  geom_point(aes(x = year2019, y = year2020, color = party, size = pct, alpha = 0.5))

ggplot(murder.old, aes(x = year2019, y = year2020, color = party, size = pct, alpha = 0.5)) + 
  geom_point() + geom_text_repel(label = murder.old$city,  size= 2.5) + 
  theme_classic() +
  ggtitle("Major City Murder, 2019-2020")

ggplot(murder.new, aes(x = year2022, y = year2023, size = pct, alpha = 0.5)) + 
  geom_point() + geom_text_repel(label = murder.new$city,  size= 2.5) + 
  theme_classic() +
  ggtitle("Major City Murder, 2022-2023")

# dumbbell
graph1 <- murder.old %>%
  ggplot(aes(x = year2019, xend = year2020, y = city)) +
  geom_dumbbell(
    colour = "#a3c4dc",
    colour_xend = "#0e668b",
    size = 4.0,
    dot_guide = TRUE,
    dot_guide_size =  0.15,
    dot_guide_colour = "grey60")
graph1

graph2 <- murder.old %>%
  ggplot(aes(x = year2019, xend = year2020, y = reorder(city, year2020))) +
  geom_dumbbell(
    colour = "#a3c4dc",
    colour_xend = "#0e668b",
    size = 2.0,
    dot_guide = TRUE,
    dot_guide_size =  0.15,
    dot_guide_colour = "grey60")
graph2

graph3 <- graph2 +
  labs(
    title = "My Title",
    x = "Murders",
    y = "Cities"
  ) +
  theme_tq() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(),
    axis.ticks = element_blank()
  )
graph3






