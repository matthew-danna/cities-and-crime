# Step 0
install.packages('tidyverse')
install.packages('tidycensus')
library(tidyverse)
library(tidycensus)

# Step 1
census_api_key("794f37c27092be60b131e4f207abcf950f227f38", 
               install = TRUE, overwrite = TRUE) # run once!
readRenviron("~/.Renviron")

# Step 2
variables <- load_variables(2023, "acs5", cache = TRUE)
variables.totals <- subset(variables, variables$label == 'Estimate!!Total:')

# Step 3
## population by state - not particularly useful
pop23 <- get_acs(geography = "state",
                 variables = "B01003_001",
                 year = 2023)

### go get the datas
pop.msa.23 <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area",
                      variables = "B01003_001",
                      year = 2023)
### do a basic calculation
pop.msa.23$diff <- abs(pop.msa.23$estimate - pop.msa.23$estimate[893])

### filter the results down
population <- subset(pop.msa.23, pop.msa.23$diff < 4000000)

##### Race by MSA
# get the datas
msa.race <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area",
                    variables = c("B02008_001", "B02009_001", "B02010_001", "B02011_001"),
                    year = 2023)
# clean the codes into names
msa.race$variable <- gsub("B02008_001", "White", msa.race$variable)
msa.race$variable <- gsub("B02009_001", "Black", msa.race$variable)
msa.race$variable <- gsub("B02010_001", "American Indian", msa.race$variable)
msa.race$variable <- gsub("B02011_001", "Asian", msa.race$variable)
# join total population to Race by MSA
msa.race.total <- msa.race %>%
  left_join(pop.msa.23, by = "GEOID")
# keep only the valuable columns
msa.race.total <- msa.race.total[c(1:4,8,10)]
# clean up the column names
names(msa.race.total) <- c("GEOID", "Name", "Race", "Race.Count",
                           "Population", "Pop.Diff")
# calculate percent of population in each MSA by Race
msa.race.total$race.PCT <- round(msa.race.total$Race.Count/msa.race.total$Population*100,2)


dc_flows <- get_flows(
  geography = "metropolitan statistical area",
  msa = 47900, # can adjust this to different areas
  year = 2020,
  geometry = TRUE)

dc.philly.atlanta <- get_flows(
  geography = "metropolitan statistical area",
  msa = c(47900,37980,12060),
  year = 2020,
  geometry = TRUE)

all.flows <- get_flows(
  geography = "metropolitan statistical area",
  year = 2020,
  geometry = TRUE)



