### PACKAGES
#install.packages('data.table')
#install.packages('R.utils')
#install.packages('sf')
#install.packages('geojsonsf')
#install.packages('tidycensus')
#install.packages('maps')
library(data.table)
library(tidyverse)
library(sf)
library(geojsonsf)
library(tidycensus)

### 2024 DATA
election.2024 <- fread("https://int.nyt.com/newsgraphics/elections/map-data/2024/national/precincts-with-results.csv.gz")
election.2024 <- separate(election.2024, 
                          GEOID, 
                          into = c("FIPS", "PRECINCT", "PRECINCT.NAME"),
                          sep = "-")
colnames(election.2024)
names(election.2024) <- c("state","FIPS","PRECINCT",         
                          "PRECINCT.NAME","votes_dem_2024","votes_rep_2024",        
                          "votes_total_2024","pct_dem_lead_2024","official_boundary")
election.2024$pct_dem_lead_2024 <- election.2024$pct_dem_lead_2024 * 100

summary.2024 <- election.2024 %>%
  group_by(FIPS) %>%
  summarise(sum(votes_dem_2024), sum(votes_rep_2024), sum(votes_total_2024))
names(summary.2024) <- c("FIPS","votes_dem_2024","votes_rep_2024","votes_total_2024")

### 2020 DATA
url.2020 <- 'https://int.nyt.com/newsgraphics/elections/map-data/2020/national/precincts-with-results.geojson.gz'
file.2020 <- gzcon(url(url.2020, 'rb'))
election.2020 <- geojson_sf(file.2020)
election.2020 <- separate(election.2020,
                          GEOID,
                          into = c("FIPS", "NAME", "EXTRA"))
colnames(election.2020)
names(election.2020) <- c("FIPS","NAME","EXTRA","votes_dem_2020","votes_rep_2020",     
                          "votes_total_2020","votes_per_sqkm_2020","pct_dem_lead_2020",
                          "geometry")
summary.2020 <- election.2020 %>%
  group_by(FIPS) %>%
  summarise(sum(votes_dem_2020), sum(votes_rep_2020), sum(votes_total_2020))
names(summary.2020) <- c("FIPS","votes_dem_2020","votes_rep_2020","votes_total_2020")

##### ALL FIPS CODES
data("fips_codes")

##### MAP WITH HIGHLIGHTED COUNTIES
fips.subset <- c(19097, 17155, 50009, 27055, 39143, 55113, 44003, 
                 55011, 19105, 46109, 19179, 55099, 51057)

maps::county.fips %>%
  as.tibble %>% 
  extract(polyname, c("region", "subregion"), "^([^,]+),([^,]+)$") ->
  fips.clean

map_data("county") %>% 
  left_join(fips.clean) ->
  fips.all

fips.all %>% 
  mutate(Counties = fips %in% fips.subset) %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill=Counties), color="gray70") +
  coord_map() +
  scale_fill_manual(values=c("TRUE"="red", "FALSE"="gray90"))
