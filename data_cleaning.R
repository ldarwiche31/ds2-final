
# Final Project: Data Cleaning  -------------------------------------------
# Name: Leila Darwiche

# load packages
library(tidyverse)
library(readxl)

# set seed (we shouldn't need it really in this process but lets keep it consistent)
set.seed(274)

# HUD initial dataset -----------------------------------------------------

hud_data <- read_xlsx("data/unprocessed/COUNTY_2020.xlsx",
                      na = c("-4", "-1", "-5", "NA")) %>%
  filter(program_label == "Summary of All HUD Programs",
         !is.na(total_units)) %>%
  select(state, county = name, code, total_units, poverty = tpoverty, minority = tminority)


# Additional datasets -----------------------------------------------------

# population
pop_data <- read_xlsx("data/unprocessed/PopulationEstimates.xlsx") %>%
  # for consistency when joining
  rename(pop = pop_2020, code = fips_code) 

# creating new output variable
hud_data <- hud_data %>%
  inner_join(pop_data, by = "code") %>%
  mutate(total_units_per_1000 = total_units/pop*1000)

# poverty: interested in percent of children (0-17) in poverty and Urban Influence code
pov_data <- read_csv("data/unprocessed/PovertyEstimates.csv") %>%
  # need to pivot... attribute to column; value to value
  pivot_wider(names_from = Attribute, values_from = Value) %>%
  select(code = FIPStxt, poverty_children = PCTPOV017_2019) %>%
  # read in and stored as numeric... lost leading and 0s and can't join without this step
  mutate(code = str_pad(as.character(code), 5, "left", "0"))

# education
edu_data <- read_csv("data/unprocessed/Education.csv") %>%
  # interested in those from 2015-2019 and the percents
  select(`FIPS Code`, (starts_with("Percent") & ends_with("19")))
  # really long names... dont want to type so will change this way
  names(edu_data) <- c("code", "edu_lt_hs", "edu_hs", "edu_some_coll", "edu_coll_plus")

# employment
employ_data <- read_csv("data/unprocessed/Unemployment.csv") %>%
  # need to pivot... attribute to column; value to value
  pivot_wider(names_from = Attribute, values_from = Value) %>%
  select(code = FIPS_Code, ends_with("2019"), metro = Metro_2013) %>%
  rename_with(~ tolower(str_remove(.x, "_2019"))) %>%
  # read in and stored as numeric... lost leading and 0s and can't join without this step
  mutate(code = str_pad(as.character(code), 5, "left", "0"))

# occupancy
occup_data <- read_csv("data/unprocessed/occupancy.csv") %>%
  mutate(GEO_ID = str_sub(GEO_ID, -5)) %>%
  select(code = GEO_ID, occupied = H1_002N, vacant = H1_003N)

# race
race_data <- read_csv("data/unprocessed/race.csv") %>%
  mutate(GEO_ID = str_sub(GEO_ID, -5)) %>%
  select(code = GEO_ID,
         white = P1_003N,
         black = P1_004N,
         native = P1_005N,
         asian = P1_006N,
         pac_isl = P1_007N,
         other = P1_008N,
         mixed = P1_009N)

# group housing
gh_data <- read_csv("data/unprocessed/group_housing.csv")  %>%
  mutate(GEO_ID = str_sub(GEO_ID, -5)) %>%
  # particularly interested in those in correctional facilities, juvenile centers, student housing, and military
  select(code = GEO_ID,
         housing_correctional = P5_003N,
         housing_juvie = P5_004N,
         housing_coll = P5_008N,
         housing_mil = P5_009N)

# urban influence
urb_data <- read_excel("data/unprocessed/UrbanInfluenceCodes2013.xls") %>%
  select(code = FIPS, urb_inf = UIC_2013)

# Joining all datasets ----------------------------------------------------

hud_data <- hud_data %>%
  left_join(pov_data, by = "code") %>%
  left_join(edu_data, by = "code") %>%
  left_join(employ_data, by = "code") %>%
  left_join(occup_data, by = "code") %>%
  left_join(race_data, by = "code") %>%
  left_join(gh_data, by = "code") %>%
  left_join(urb_data, by = "code") %>%
  # make all of these into percentages and rescale here rather than the recipe...
  # part of the reason is for consistency going into the recipe since some are already as percentage points
  mutate(civilian_labor_force = civilian_labor_force/pop*100,
         occupied = occupied/pop*1000,
         vacant = vacant/pop*1000,
         white = white/pop*100,
         black = black/pop*100,
         native = native/pop*100,
         asian = asian/pop*100,
         pac_isl = pac_isl/pop*100,
         other = other/pop*100,
         mixed = mixed/pop*100,
         housing_correctional = housing_correctional/pop*100,
         housing_juvie = housing_juvie/pop*100,
         housing_coll = housing_coll/pop*100,
         housing_mil = housing_mil/pop*100) %>%
  select(-unemployed, -employed)


# Write out dataset -------------------------------------------------------

write_rds(hud_data, "data/processed/hud_data.RDS")


