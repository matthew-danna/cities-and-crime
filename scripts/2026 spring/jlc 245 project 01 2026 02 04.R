# Step 0: Libraries
#install.packages('tidyverse')
#install.packages('explore')
#install.packages('GGally')

library(tidyverse)
library(explore)
library(GGally)

# Step 1: examine data online

# Step 2
url <- "https://opendata.dc.gov/datasets/DCGIS::crime-incidents-in-"
years <- c(2008:2026)
full.urls <- paste0(url, years, ".csv")
dc.data <- data.frame()

for(file in full.urls) {
  tmp.data <- read.csv(file, stringsAsFactors = FALSE)
  dc.data <- rbind(tmp.data, dc.data)
}

# Step 3
dc.data <- separate(dc.data, REPORT_DAT, into = c("date", "time"), sep = " ")

# Step 4
dc.data$date <- as.Date(dc.data$date, format = "%Y/%m/%d")
dc.data$hour <- substr(dc.data$time, 0, 2)
dc.data$hour <- as.numeric(dc.data$hour)
dc.data$dow <- weekdays(dc.data$date)
dc.data$week <- format(dc.data$date, "%U")
dc.data$week <- as.numeric(dc.data$week) 
dc.data$months <- months(dc.data$date)
dc.data$month <- month(dc.data$date)
dc.data$year <- substr(dc.data$date, 0, 4)

# Step 5
explore(dc.data)

### summary table
sum.offense <- dc.data %>%
  group_by(OFFENSE) %>%
  summarise(COUNT = n()) %>%
  mutate(PCT = round(COUNT/sum(COUNT)*100,2))

sum.offense.shift <- dc.data %>%
  group_by(OFFENSE, SHIFT) %>%
  summarise(COUNT = n()) %>%
  mutate(PCT = round(COUNT/sum(COUNT)*100,2))

# Step 6
ggplot()
ggplot(dc.data, aes(SHIFT))
ggplot(dc.data, aes(SHIFT)) + geom_bar(stat = "count")

ggplot(dc.data, aes(SHIFT)) + 
  geom_bar(stat = "count", aes(fill = OFFENSE))

ggplot(dc.data, aes(SHIFT)) + 
  geom_bar(stat = "count", aes(fill = OFFENSE)) +
  theme_classic()

ggplot(dc.data, aes(SHIFT)) + 
  geom_bar(stat = "count", aes(fill = OFFENSE), 
           position = position_stack(reverse = TRUE)) + theme_classic() + 
  theme(legend.position = "bottom")

ggplot(dc.data, aes(SHIFT)) + 
  geom_bar(stat = "count", aes(fill = OFFENSE)) + 
  theme_classic() + 
  facet_wrap(~DISTRICT)

### formatting
ggplot(dc.data, aes(SHIFT)) + 
  geom_bar(stat = "count") + 
  theme_classic() +
  labs(
    title = "My First Graph",
    x = "x axis label here",
    y = "y axis label here"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(),
    axis.ticks = element_blank()
  )

### Better Graphs!
dc.data$TYPE <- case_when(
  dc.data$OFFENSE %in%
    c(
      "ARSON",
      "BURGLARY",
      "MOTOR VEHICLE THEFT",
      "THEFT F/AUTO",
      "THEFT/OTHER") ~ "Property",
  dc.data$OFFENSE %in%
    c("ASSAULT W/DANGEROUS WEAPON", "HOMICIDE", 
      "ROBBERY", "SEX ABUSE")
  ~ "Person")

# Crimes per hour
dc.data %>% ggplot() +
  geom_line(aes(x = hour), stat = "count")

dc.data %>% ggplot() +
  geom_line(aes(x = hour), stat = "count", 
            group = 1, color = "blue", size = 1)

dc.data %>% ggplot() +
  geom_line(aes(x = hour), stat = "count", group = 1, 
            color = "blue", size = 1) +
  labs(title = "DC Crimes by Hour Reported \n2008 - 2026 YTD", 
       x = "Hour Reported", 
       y = "Number of Crimes (Thousands)", 
       fill = "Type of Crime")

dc.data %>% ggplot() +
  geom_line(aes(x = hour), stat = "count", group = 1, 
            color = "blue", size = 1) +
  labs(title = "DC Crimes by Hour Reported \n2008 - 2026 YTD", 
       x = "Hour Reported", 
       y = "Number of Crimes (Thousands)", 
       fill = "Type of Crime") +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray"),
        panel.background = element_blank())

dc.data %>% ggplot() +
  geom_line(aes(x = hour), stat = "count", group = 1, 
            color = "blue", size = 1) +
  labs(title = "DC Crimes by Hour Reported \n2008 - 2026 YTD", 
       x = "Hour Reported", 
       y = "Number of Crimes (Thousands)") +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray"),
        panel.background = element_blank()) +
  scale_y_continuous(limits = c(0, 35000), 
                     breaks = c(0, 5000,10000,15000,20000,25000,30000,35000), 
                     labels = c(0,5,10,15,20,25,30,35))

# Crimes per District
dc.data %>% filter(DISTRICT != "NA") %>%
  ggplot() +
  geom_bar(aes(x = as.factor(DISTRICT), fill = TYPE), 
           stat = "count") +
  labs(title = "Crimes by DC Police District \n2010 - 2026 YTD", 
       x = "District", 
       y = "Number of Crimes (Thousands)", fill = "Type of Crime") +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray"),
        panel.background = element_blank()) +
  scale_y_continuous(limits = c(0, 125000), 
                     breaks = c(0, 25000,50000,75000,100000,125000), 
                     labels = c(0,25,50,75,100,125))

# Persons crime by Method
dc.data %>% filter(TYPE == "Person") %>%
  ggplot() +
  geom_bar(aes(x = OFFENSE, fill = METHOD), position = "fill") +
  labs(title = "DC Persons Crimes by Method \n20xx - 2026 YTD", x = "Crime", y = "Proportion of Crime", fill = "Method") +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray"),
        panel.background = element_blank()) +
  scale_x_discrete(labels = c("Assault", "Homicide", "Robbery", "Sex Abuse")) +
  scale_y_continuous(labels = c("0%", "25%", "50%", "75%", "100%")) +
  scale_fill_manual(values = c("#1b9e77", "#d95f02","#7570b3"), labels = c("Gun", "Knife", "Other"))

