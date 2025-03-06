# Step 0
install.packages('tidyverse')
install.packages('tidycensus')
library(tidyverse)
library(tidycensus)

install.packages('ggrepel')
library(ggrepel)

# Step 1
census_api_key("794f37c27092be60b131e4f207abcf950f227f38", 
               install = TRUE, overwrite = TRUE) # run once!
readRenviron("~/.Renviron")

# Step 2
variables <- load_variables(2023, "acs5", cache = TRUE)
variables.totals <- subset(variables, variables$label == 'Estimate!!Total:')

# Step 3
### go get the datas
pop.msa.23 <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area",
                      variables = "B01003_001",
                      year = 2023)
### do a basic calculation
pop.msa.23$diff <- abs(pop.msa.23$estimate - pop.msa.23$estimate[893])

### filter the results down
population <- subset(pop.msa.23, pop.msa.23$diff < 1000000)

##### Race by MSA
# get the datas
msa.race <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area",
                    variables = "B02008_001",
                    year = 2023)
# clean the codes into names
msa.race$variable <- gsub("B02008_001", "White", msa.race$variable)

# join total population to Race by MSA
msa.race.total <- msa.race %>%
  left_join(pop.msa.23, by = "GEOID")

# keep only the valuable columns
msa.race.total <- msa.race.total[c(1,2,4,8,10)]

# clean up the column names
names(msa.race.total) <- c("GEOID", "MSA.Name", "Population.White",
                           "Population.Total", "Population.Diff")

# calculate percent of population in each MSA by Race
msa.race.total$Race.PCT <- round(msa.race.total$Population.White/msa.race.total$Population.Total*100,2)

# compare Race PCT of DC to all other MSAs
msa.race.total$Race.Diff <- abs(msa.race.total$Race.PCT - msa.race.total$Race.PCT[893])

# Income
msa.income <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area",
                    variables = "B19019_001",
                    year = 2023)

msa.income$income.max <- msa.income$estimate + 0.5*msa.income$moe
msa.income$income.min <- msa.income$estimate - 0.5*msa.income$moe

msa.income$diff <- abs(msa.income$estimate - msa.income$estimate[893])

# Education
msa.education <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area",
                      variables = "C15010_001",
                      year = 2023)

msa.education <- msa.education %>%
  left_join(pop.msa.23, by = 'GEOID')

msa.education$education.PCT <- round(msa.education$estimate.x/msa.education$estimate.y * 100,2)

# Poverty
msa.poverty <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area",
                         variables = "B17026_001",
                         year = 2023)
msa.poverty$diff <- abs(msa.poverty$estimate - msa.poverty$estimate[893])

msa.poverty <- msa.poverty %>%
  left_join(pop.msa.23, by = "GEOID")

msa.poverty$poverty.PCT <- round(msa.poverty$estimate.x/msa.poverty$estimate.y * 100,2)

msa.poverty$pct.diff <- abs(msa.poverty$poverty.PCT - msa.poverty$poverty.PCT[893])

msa.poverty.subset <- subset(msa.poverty, msa.poverty$diff.x < 1000000 &
                               msa.poverty$pct.diff < 1) 

ggplot(msa.poverty.subset, aes(x = estimate.x, y = poverty.PCT)) +
  geom_point() +
  geom_text_repel(label = msa.poverty.subset$NAME.x,  size= 2.0) +
  labs(title = "My Graph",
       x = "Number of Households",
       y = "Percent of Population")

# Flows
all.flows <- get_flows(
  geography = "metropolitan statistical area",
  year = 2020,
  geometry = TRUE)



