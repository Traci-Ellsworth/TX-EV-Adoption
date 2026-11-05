# libraries --------------------------------------------------------------------
library(tidyverse)
library(tidycensus)
library(sf)



# load.csv ---------------------------------------------------------------------
ev <- read.csv("ev-adoption.csv")  # num vehicles, num EVs, adoption rate
vot <- read.csv("2024-voting.csv") # 2024 election - % repub. votes for president from Harvard dataverse
vot <- vot %>% select(c(county_fips, pct.rep))
chg <- read.csv("charger-locations.csv")  # lat/lon of EV chargers from https://afdc.energy.gov/corridors



# acs --------------------------------------------------------------------------
acs_vars <- c(
  pop       = "B03003_001E",  # total population
  med_age   = "B01002_001E",  # median age  
  hisp      = "B03003_003E",  # Hispanic or Latino
  income    = "B19013_001E",  # median household income
  
  ba        = "B15003_022E",  # bachelor's
  ma        = "B15003_023E",  # master's
  prof      = "B15003_024E",  # professional degree
  doc       = "B15003_025E",  # doctorate
  edu_tot   = "B15003_001E",  # population 25+  (denominator for % with degree)
  
  sfd       = "B25024_002E",  # single-family detached
  units_tot = "B25024_001E",  # total housing units  (denominator for % sfd)
  
  comm60    = "B08303_012E",  # commute 60-89 minutes
  comm90    = "B08303_013E",  # commute 90+ minutes
  comm_tot  = "B08303_001E"   # denominator for commuters
)

acs <- get_acs(geography = "county", state = "TX", variables = acs_vars,
               year = 2024, survey = "acs5", geometry = TRUE, output = "wide")

cov_acs <- acs %>% transmute(GEOID = GEOID, NAME = NAME, pop = pop, med_age = med_age, med_inc = income,
    pct_deg   = (ba + ma + prof + doc) / edu_tot,
    pct_sfd  = sfd / units_tot,
    pct_hisp = hisp / pop,
    pct_long_comm  = (comm60 + comm90) / comm_tot
  ) %>% st_transform(3081)  #TX statewide crs
head(cov_acs)



# chargers ---------------------------------------------------------------------
# Charger Locations - Get number of chargers within 10 miles of each county's boundaries
chg_sf <- st_as_sf(chg, coords=c("Longitude","Latitude"), crs=4326) %>% st_transform(3081)   #make spatial - TX crs
buffer <- 16093.4    #10 miles in meters
buff.bound <- st_buffer(st_geometry(cov_acs), dist = buffer)   #buffer around each county boundary)
chg.in.cty <- st_intersects(buff.bound, chg_sf)   #number of chargers within each county's buffered boundary
sum(is.na(chg_sf$EV.DC.Fast.Count))  # any NA's for number of chargers at each station?
num.chg <- data.frame(GEOID = cov_acs$GEOID, 
                      n.stations = lengths(chg.in.cty),
                      n.chargers = map_dbl(chg.in.cty, ~ sum(chg_sf$EV.DC.Fast.Count[.x], na.rm=TRUE)))
head(num.chg)



# join.data --------------------------------------------------------------------
#join EV data, and covariates from ACS, voting records, and charger locations
vot <- rename(vot, "GEOID" = "county_fips")
vot$GEOID <- as.character(vot$GEOID)
ev <- rename(ev, "GEOID" = "FIPS.Code")
ev$GEOID <- as.character(ev$GEOID)
df <- cov_acs %>% left_join(vot, by="GEOID") %>% 
                  left_join(num.chg, by="GEOID") %>%
                  inner_join(ev, by="GEOID")
print(colSums(is.na(st_drop_geometry(df))))
summary(df)
print(car::vif(lm(Electric.Vehicles/All.Vehicles ~ med_age + med_inc + pct_deg + pct_sfd + pct_hisp + pct_long_comm + pct.rep + n.chargers, data=df)))
