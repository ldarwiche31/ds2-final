
# Final Project: Initial Setup + EDA --------------------------------------
# Name: Leila Darwiche

# Since I don't have a ton of data, I want to use a random sample of my train data
# to actually go about my EDA. Thus, I am going to have both my initial split and EDA work within this file.

# Load packages
library(tidymodels)
library(tidyverse)
tidymodels_prefer()

# Set seed (will use across all scripts)
set.seed(274)

# Load data
hud_data <- read_rds("data/processed/hud_data.RDS")

# Data checks -------------------------------------------------------------

# Data quality check: let us just look at our NA values
hud_data %>%
  naniar::miss_var_summary() %>%
  View()
# most of our variables have less than a 5% missing rate...
# as this is very low, we can try to resolve it in the recipe stage using a step_impute
# we should also drop the one variable that has a null outcome

# Data quality check: outcome variable
ggplot(hud_data, aes(x = total_units_per_1000)) +
  geom_histogram()

ggplot(hud_data, aes(x = total_units_per_1000)) +
  geom_density()
# our data is still pretty skewed, so a log transformation on the outcome
# would be an important thing to do

# Resolve factors + outcome
hud_data <- hud_data %>%
  mutate(
    # this is somewhat ordered in that one is very metropolitan and 12 is the least metropolitan... ordered factor thus makes sense
    urb_inf = factor(urb_inf, ordered = TRUE),
    metro = factor(metro,
                   levels = c(0,1),
                   labels = c("no", "yes")),
    # make outcome logged (using base 10 because its in units of per 1000)
    log_total_units_per_1000 = log10(total_units_per_1000)) %>%
  drop_na(log_total_units_per_1000)

# Splitting the data ------------------------------------------------------

split <- initial_split(hud_data, .8, strata = log_total_units_per_1000)
hud_test <- testing(split)
hud_train <- training(split)

hud_folds <- hud_train %>%
  vfold_cv(v = 5, repeats = 10, strata = log_total_units_per_1000)

# EDA ---------------------------------------------------------------------

# dataset for EDA
eda <- hud_train %>% slice_sample(prop = 0.5)

# we have a lot of numeric variables... let us just quickly look at the correlation of these variables with the outcome variable
hud_numeric <- map(eda, is.numeric)
hud_numeric <- unlist(hud_numeric)

cor(eda[hud_numeric], eda$log_total_units_per_1000, use = "pairwise.complete.obs")

# there are a decent amount of variables that have some low/moderate correlation with the outcome variable
# particularly those dealing with race and dealing with poverty. I think it is important to reconcile
# that the reason why some of these numbers may be low is perhaps because of other relationships in the data

# let us explore a few particular variables of interest
# poverty
ggplot(eda, aes(x = poverty, y = log_total_units_per_1000)) +
  geom_point() +
  geom_smooth( method = lm,
               formula = y ~ splines::ns(x, df = 4),
               color = "lightblue") # could be good to add splines to this variable

ggplot(eda, aes(x = poverty, y = log_total_units_per_1000)) +
  geom_point() +
  geom_smooth( method = lm,
               formula = y ~ splines::ns(x, df = 10),
               color = "lightblue") # def too many... 4 probably good

ggplot(eda, aes(x = poverty_children, y = log_total_units_per_1000)) +
  geom_point() +
  geom_smooth(se = FALSE) # upward trend... def don't need splines

# there is a strong linear trend in those with poverty values below 40
# however, there is a drop off at some point.. this makes a lot of sense
# perhaps because of budget and allocated spending... at some point demand will
# go over availability

# race/minority
ggplot(eda, aes(x = minority, y = log_total_units_per_1000)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

ggplot(eda) +
  geom_point(aes(x = white, y = log_total_units_per_1000, color = "white")) +
  geom_point(aes(x = black, y = log_total_units_per_1000, color = "black")) +
  geom_point(aes(x = asian, y = log_total_units_per_1000, color = "asian")) +
  geom_smooth(aes(x = white, y = log_total_units_per_1000, color = "white"),
              method = lm,
              formula = y ~ splines::ns(x, df = 2)) +
  geom_smooth(aes(x = black, y = log_total_units_per_1000, color = "black"),
              method = lm,
              formula = y ~ splines::ns(x, df = 2)) +
  geom_smooth(aes(x = asian, y = log_total_units_per_1000, color = "asian"),
              method = lm,
              formula = y ~ splines::ns(x, df = 2)) +
  labs(x = "Race Percentage", color = "Race") # splines could be good for white and perhaps asian

ggplot(eda, aes(x = (minority))) +
  geom_histogram() # log could be good

ggplot(eda, aes(x = log10(minority))) +
  geom_histogram()

# looks good to have log for most minorities as well (but not white)


# housing: brief look at training set shows lots of 0s for these...
# better to maybe encode yes or no
ggplot(eda, aes(y = log_total_units_per_1000, x = housing_juvie)) +
  geom_point()

ggplot(eda, aes(y = log_total_units_per_1000, x = housing_juvie)) +
  geom_point()

ggplot(eda, aes(y = log_total_units_per_1000, x = housing_coll)) +
  geom_point()

ggplot(eda, aes(y = log_total_units_per_1000, x = housing_mil)) +
  geom_point()

# employment
ggplot(eda, aes(x = median_household_income, y = log_total_units_per_1000, color = unemployment_rate)) +
  geom_point()
# seems like there is a negative relationship between the two
ggplot(eda, aes(x= median_household_income)) +
  geom_histogram() # not super skewed

ggplot(eda, aes(x = civilian_labor_force, y = log_total_units_per_1000, color = unemployment_rate)) +
  geom_point()
# again general negative relationship
ggplot(eda, aes(x= civilian_labor_force)) +
  geom_histogram() # not super skewed

ggplot(eda, aes(x= unemployment_rate)) +
  geom_histogram() # super skewed

# education
ggplot(eda, aes(x = edu_lt_hs, y = log_total_units_per_1000)) +
  geom_point() # upward (ish?) trend

ggplot(eda, aes(x = edu_some_coll, y = log_total_units_per_1000)) +
  geom_point() # downward trend

# none of them are that skewed
ggplot(eda, aes(x = edu_lt_hs)) +
  geom_histogram()
ggplot(eda, aes(x = edu_hs)) +
  geom_histogram()
ggplot(eda, aes(x = edu_some_coll)) +
  geom_histogram()
ggplot(eda, aes(x = edu_coll_plus)) +
  geom_histogram() # except for this one

# housing
ggplot(eda, aes(x = occupied, y = log_total_units_per_1000)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(eda, aes(x = log10(vacant), y = log_total_units_per_1000)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(eda, aes(x = occupied)) +
  geom_histogram()

ggplot(eda, aes(x = log10(vacant))) +
  geom_histogram()

# assessing categoricals:
ggplot(eda %>% group_by(urb_inf) %>% summarize(median = median(log_total_units_per_1000)),
       aes(x = urb_inf, y = median)) +
  geom_col() # less urban = more... until we get to really rural areas

ggplot(eda, aes(x = urb_inf)) +
  geom_bar()

ggplot(eda, aes(y = urb_inf, x = log_total_units_per_1000)) +
  geom_boxplot()

ggplot(eda %>% group_by(metro) %>% summarize(median = median(log_total_units_per_1000)),
       aes(x = metro, y = median)) +
  geom_col() # less urban = more... until we get to really rural areas

ggplot(eda, aes(x = metro)) +
  geom_bar()

ggplot(eda, aes(y = metro, x = log_total_units_per_1000)) +
  geom_boxplot() # non metro are a lot more concentrated

# have missing-ness in poverty.. what can be used to predict it?
cor(eda[hud_numeric], eda$poverty, use = "pairwise.complete.obs")
# edu_lt_hs, ed_hs, ed_some_coll, civilian_labor_force, unemployment_rate, white, black, mixed

# have missing-ness in poverty_children.. what can be used to predict it?
cor(eda[hud_numeric], eda$poverty_children, use = "pairwise.complete.obs")
# edu_lt_hs, edu_hs, edu_some_coll, edu_coll_plus, civilian_labor_force, unemployment_rate, white, black, asian

# have missing-ness in median_household_income
cor(eda[hud_numeric], eda$median_household_income, use = "pairwise.complete.obs")
# edu_lt_hs, edu_hs, edu_coll_plus, civilian_labor_force, unemployment_rate, asian

# have missing-ness in med_hh_income_percent_of_state_total
cor(eda[hud_numeric], eda$med_hh_income_percent_of_state_total, use = "pairwise.complete.obs")
# edu_lt_hs, edu_hs, edu_coll_plus, civilian_labor_force, unemployment_rate, asian

# Conclusions:

# in general, variables that assessed poverty, minority populations, levels of unemployment
# and education status (hs and below) had upward trends

# in general, variables that assessed wealth had downward trends

# we should encode housing as yes and no

# some variables splines could be good... and some log transforms

# Write out datasets ------------------------------------------------------

save(split, hud_folds, file = "data/processed/splits.RData")

