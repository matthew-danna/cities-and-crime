# Step 0: libraries
install.packages('tidycensus')
library(tidyverse)
library(tidycensus)

# Step 1: get the API key installed
census_api_key("794f37c27092be60b131e4f207abcf950f227f38",
               install = TRUE, overwrite = TRUE)

# Step 2: get possible datasets to work with
variables <- load_variables(2022, "acs5", cache = TRUE)
variables.totals <- subset(variables, variables$label == 'Estimate!!Total:')

# Step 3: write some queries
# state populations
pop22 <- get_acs(geography = "state",
                 variables = "B01003_001",
                 year = 2022)

# MSA populations
pop.msa.22 <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area",
                      variables = "B01003_001",
                      year = 2022)

pop.msa.22$diff <- abs(pop.msa.22$estimate - pop.msa.22$estimate[897])
population <- subset(pop.msa.22, pop.msa.22$diff < 2000000)

# MSA populations by race
msa.race <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area",
                    variables = c("B02008_001", "B02009_001", "B02010_001", 
                                  "B02011_001"),
                    year = 2022)
msa.race$variable <- gsub("B02008_001", "White", msa.race$variable)
msa.race$variable <- gsub("B02009_001", "Black", msa.race$variable)
msa.race$variable <- gsub("B02010_001", "American Indian", msa.race$variable)
msa.race$variable <- gsub("B02011_001", "Asian", msa.race$variable)

msa.race.total <- msa.race %>%
  left_join(pop.msa.22, by = "GEOID")
msa.race.total <- msa.race.total[c(1:5,8,10)]
names(msa.race.total) <- c("GEOID", "Name", "Race", "Race.Count", 
                           "Race.MOE", "Population", "Pop.Diff")
msa.race.total$race.PCT <- msa.race.total$Race.Count/msa.race.total$Population*100

msa.race.black <- subset(msa.race.total, msa.race.total$Race == 'Black')
msa.race.white <- subset(msa.race.total, msa.race.total$Race == 'White')

msa.race.black$race.diff <- abs(msa.race.black$race.PCT - msa.race.black$race.PCT[897])
msa.race.white$race.diff <- abs(msa.race.white$race.PCT - msa.race.white$race.PCT[897])

msa.race.all <- rbind(msa.race.black, msa.race.white)

# get component datasets
state_components <- get_estimates(geography = "state", product = "components")

# county components
sex.age.hisp <- get_estimates(geography = "county", 
                              product = "characteristics", 
                              breakdown = c("SEX", "AGEGROUP", "HISP"),  
                              breakdown_labels = TRUE)

# age groups by county
county.age <- get_estimates(geography = "county",
                            product = "characteristics",
                            breakdown = c("AGEGROUP"),  
                            breakdown_labels = TRUE)

# flows
dc_flows <- get_flows(
  geography = "metropolitan statistical area",
  msa = 47900, # can adjust this to different areas
  year = 2020,
  geometry = TRUE
)

# median household income
income.22 <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area",
                      variables = "B19019_001",
                      year = 2022)


