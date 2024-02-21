# Step 0: Packages
library(tidyverse)
library(explore)
install.packages('ggrepel')
library(ggrepel)

# Step 1: Get Data
ucr <- read.csv("https://www.dropbox.com/scl/fi/bh0fr9axs5arbpiye7rzs/UCR65_22b.csv?rlkey=py4u1gmw8dol5xxx0rek7as5q&dl=1", 
                stringsAsFactors = FALSE)

# Step 2: Examine Data
explore(ucr)

# Step 3: Summarize Data
ucr.county <- ucr %>%
  group_by(County) %>%
  summarise(sum(MRD), sum(CLR))

names(ucr.county) <- c("county", "murders", "cleared")

ucr.county$rate <- round((ucr.county$cleared/ucr.county$murders *100), 0)

ucr.county.year <- ucr %>%
  group_by(County, YEAR) %>%
  summarise(sum(MRD), sum(CLR))
names(ucr.county.year) <- c("county", "year", "murders", "cleared")
ucr.county.year$rate <- round((ucr.county.year$cleared/ucr.county.year$murders *100), 2)

# Step 4: Subset
ucr.dc <- subset(ucr.county.year, county == 'District of Columbia')
explore(ucr.dc)

ucr.county.recent <- ucr %>%
  group_by(County) %>%
  filter(YEAR > '2015') %>%
  summarise(sum(MRD), sum(CLR))
names(ucr.county.recent) <- c("county", "murders", "cleared")
ucr.county.recent$rate <- round((ucr.county.recent$cleared/ucr.county.recent$murders *100), 2)


# Step 5: Graphs
### Bar graph
#ggplot(ucr.county.recent, aes(murders)) + geom_bar(stat = "identity")

### Scatter plot
ggplot(ucr.county) +
  geom_point(aes(x = murders, y = cleared))
ggplot(ucr.county) +
  geom_point(aes(x = murders, y = cleared), color = 'blue')
ggplot(ucr.county) +
  geom_point(aes(x = murders, y = cleared, color = rate))
ggplot(ucr.county) +
  geom_point(aes(x = murders, y = cleared, size = rate), color = 'red')
ggplot(ucr.county) +
  geom_point(aes(x = murders, y = cleared, alpha = rate))
ggplot(ucr.county) +
  geom_point(aes(x = murders, y = cleared, alpha = rate)) +
  labs(alpha = "Clearance Rate") +
  ggtitle("My first scatterplot") +
  theme_classic()
ggplot(ucr.county, aes(x = murders, y = cleared, alpha = rate)) +
  geom_point() + 
  labs(
    title = "My first scatterplot",
    subtitle = "By: me",
    caption = "Source: data source here",
    alpha = "Clearance Rate"
  ) +
  theme_classic() +
  geom_text_repel(label = ucr.county$county,  size= 1.0)


# NEW CONTENT FROM 21 FEBRUARY 2024
## Making a third scatterplot
ucr.recent.subset <- subset(ucr.county.recent, ucr.county.recent$murders > 600)

ggplot(ucr.recent.subset, aes(x = murders, y = cleared, alpha = rate, size = 2.0)) +
  geom_point() + 
  labs(
    title = "My second scatterplot",
    subtitle = "By: me",
    caption = "Source: data source here",
    alpha = "Clearance Rate"
  ) +
  theme_classic() +
  geom_text_repel(label = ucr.recent.subset$county,  size= 1.0)




