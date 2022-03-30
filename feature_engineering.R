
# Final Project: Feature Engineering --------------------------------------
# Name: Leila Darwiche

# Load packages
library(tidymodels)
library(tidyverse)
tidymodels_prefer()

# Set seed (will use across all scripts)
set.seed(274)

# Load data
load("data/processed/splits.RData")
hud_train <- training(split)

# Want to explore two recipes:
# one - just ppl info (employment, edu, minority, poverty, median hh income)
# two - housing and area characteristics (housing, occupied, vacant, metro)

base_recipe_log <- recipe(log_total_units_per_1000~., hud_train) %>%
  step_rm(state, county, code, total_units, pop, total_units_per_1000)

# Recipe One: people demographics -----------------------------------------

recipe_ppl <- base_recipe %>%
  # let us remove area/housing characteristics
  step_rm(metro, urb_inf, occupied, vacant, housing_correctional, housing_juvie, housing_coll, housing_mil) %>%
  # we still have missing variables... these are all in numeric data
  # we can try to get minority with just a simple 100 - white...
  step_mutate(minority = case_when(is.na(minority) ~ 100 - white,
                                   TRUE ~ minority)) %>%
  # there may be reason to think that similarly composed counties would have similar
  # characteristics in terms of poverty, unemployment, etc. can impute that way...
  # explored in EDA what variables were good predictors of these... linear regression bc easier to understand
  step_impute_linear(poverty,
                     impute_with = imp_vars(edu_lt_hs, edu_hs, edu_some_coll, civilian_labor_force, unemployment_rate, white, black, mixed)) %>%
  step_impute_linear(poverty_children,
                     impute_with = imp_vars(edu_lt_hs, edu_hs, edu_some_coll, edu_coll_plus, civilian_labor_force, unemployment_rate, white, black, asian)) %>%
  step_impute_linear(median_household_income,
                     impute_with = imp_vars(edu_lt_hs, edu_hs, edu_coll_plus, civilian_labor_force, unemployment_rate, asian)) %>%
  step_impute_linear(med_hh_income_percent_of_state_total,
                     impute_with = imp_vars(edu_lt_hs, edu_hs, edu_coll_plus, civilian_labor_force, unemployment_rate, asian)) %>%
  # let us log transform the variables we need, use base 10, offset (don't know if we have 0s in testing)
  step_log(black, asian, pac_isl, other, mixed, native, minority, edu_coll_plus, unemployment_rate,
           base = 10,
           offset = 1) %>%
  # last things are to drop predictors without a lot variation in a sample to avoid errors and to scale/center
  step_nzv(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  # add splines to variables
  step_ns(white, deg_free = 2) %>%
  step_ns(poverty, deg_free = 4)



# Recipe Two: all variables (people + area + housing) ---------------------

recipe_all <- base_recipe %>%
  # I will skip the rationale for all the steps above but include rationale for new variables in these steps
  step_mutate(minority = case_when(is.na(minority) ~ 100 - white,
                                  TRUE ~ minority),
              # need to make the housing variables into factors of 0 and 1
              housing_correctional = factor(case_when(housing_correctional == 0 ~ 0,
                                                      TRUE ~ 1),
                                            levels = c(0, 1),
                                            labels = c("no", "yes")),
              housing_juvie = factor(case_when(housing_juvie == 0 ~ 0,
                                                      TRUE ~ 1),
                                            levels = c(0, 1),
                                            labels = c("no", "yes")),
              housing_coll = factor(case_when(housing_coll == 0 ~ 0,
                                                      TRUE ~ 1),
                                            levels = c(0, 1),
                                            labels = c("no", "yes")),
              housing_mil = factor(case_when(housing_mil == 0 ~ 0,
                                                      TRUE ~ 1),
                                            levels = c(0, 1),
                                            labels = c("no", "yes"))) %>%
  # imputations from above
  step_impute_linear(poverty,
                     impute_with = imp_vars(edu_lt_hs, edu_hs, edu_some_coll, civilian_labor_force, unemployment_rate, white, black, mixed)) %>%
  step_impute_linear(poverty_children,
                     impute_with = imp_vars(edu_lt_hs, edu_hs, edu_some_coll, edu_coll_plus, civilian_labor_force, unemployment_rate, white, black, asian)) %>%
  step_impute_linear(median_household_income,
                     impute_with = imp_vars(edu_lt_hs, edu_hs, edu_coll_plus, civilian_labor_force, unemployment_rate, asian)) %>%
  step_impute_linear(med_hh_income_percent_of_state_total,
                     impute_with = imp_vars(edu_lt_hs, edu_hs, edu_coll_plus, civilian_labor_force, unemployment_rate, asian)) %>%
  # log10 including a few new
  step_log(black, asian, pac_isl, other, mixed, native, minority, edu_coll_plus, unemployment_rate, vacant,
           base = 10,
           offset = 1) %>%
  # create dummy variables plus account for folding issues
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  # last things are to drop predictors without a lot variation in a sample to avoid errors and to scale/center
  step_nzv(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  # add splines to variables
  step_ns(white, deg_free = 2) %>%
  step_ns(poverty, deg_free = 4)

# Recipe One 2.0: people demographics w/ no log, no one hot -----------------------------------------

# base recipe with no log:
base_recipe <- recipe(total_units_per_1000~., hud_train) %>%
  step_rm(state, county, code, total_units, pop, log_total_units_per_1000)

recipe_ppl_new <- base_no_log_recipe %>%
  # let us remove area/housing characteristics
  step_rm(metro, urb_inf, occupied, vacant, housing_correctional, housing_juvie, housing_coll, housing_mil) %>%
  # we still have missing variables... these are all in numeric data
  # we can try to get minority with just a simple 100 - white...
  step_mutate(minority = case_when(is.na(minority) ~ 100 - white,
                                   TRUE ~ minority)) %>%
  # there may be reason to think that similarly composed counties would have similar
  # characteristics in terms of poverty, unemployment, etc. can impute that way...
  # explored in EDA what variables were good predictors of these... linear regression bc easier to understand
  step_impute_linear(poverty,
                     impute_with = imp_vars(edu_lt_hs, edu_hs, edu_some_coll, civilian_labor_force, unemployment_rate, white, black, mixed)) %>%
  step_impute_linear(poverty_children,
                     impute_with = imp_vars(edu_lt_hs, edu_hs, edu_some_coll, edu_coll_plus, civilian_labor_force, unemployment_rate, white, black, asian)) %>%
  step_impute_linear(median_household_income,
                     impute_with = imp_vars(edu_lt_hs, edu_hs, edu_coll_plus, civilian_labor_force, unemployment_rate, asian)) %>%
  step_impute_linear(med_hh_income_percent_of_state_total,
                     impute_with = imp_vars(edu_lt_hs, edu_hs, edu_coll_plus, civilian_labor_force, unemployment_rate, asian)) %>%
  # let us log transform the variables we need, use base 10, offset (don't know if we have 0s in testing)
  step_log(black, asian, pac_isl, other, mixed, native, minority, edu_coll_plus, unemployment_rate,
           base = 10,
           offset = 1) %>%
  # last things are to drop predictors without a lot variation in a sample to avoid errors and to scale/center
  step_nzv(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  # add splines to variables
  step_ns(white, deg_free = 2) %>%
  step_ns(poverty, deg_free = 4)



# Recipe Two 2.0: all variables (people + area + housing) w/ no log, no one hot ---------------------

recipe_all_new <- base_no_log_recipe %>%
  # I will skip the rationale for all the steps above but include rationale for new variables in these steps
  step_mutate(minority = case_when(is.na(minority) ~ 100 - white,
                                   TRUE ~ minority),
              # need to make the housing variables into factors of 0 and 1
              housing_correctional = factor(case_when(housing_correctional == 0 ~ 0,
                                                      TRUE ~ 1),
                                            levels = c(0, 1),
                                            labels = c("no", "yes")),
              housing_juvie = factor(case_when(housing_juvie == 0 ~ 0,
                                               TRUE ~ 1),
                                     levels = c(0, 1),
                                     labels = c("no", "yes")),
              housing_coll = factor(case_when(housing_coll == 0 ~ 0,
                                              TRUE ~ 1),
                                    levels = c(0, 1),
                                    labels = c("no", "yes")),
              housing_mil = factor(case_when(housing_mil == 0 ~ 0,
                                             TRUE ~ 1),
                                   levels = c(0, 1),
                                   labels = c("no", "yes"))) %>%
  # imputations from above
  step_impute_linear(poverty,
                     impute_with = imp_vars(edu_lt_hs, edu_hs, edu_some_coll, civilian_labor_force, unemployment_rate, white, black, mixed)) %>%
  step_impute_linear(poverty_children,
                     impute_with = imp_vars(edu_lt_hs, edu_hs, edu_some_coll, edu_coll_plus, civilian_labor_force, unemployment_rate, white, black, asian)) %>%
  step_impute_linear(median_household_income,
                     impute_with = imp_vars(edu_lt_hs, edu_hs, edu_coll_plus, civilian_labor_force, unemployment_rate, asian)) %>%
  step_impute_linear(med_hh_income_percent_of_state_total,
                     impute_with = imp_vars(edu_lt_hs, edu_hs, edu_coll_plus, civilian_labor_force, unemployment_rate, asian)) %>%
  # log10 including a few new
  step_log(black, asian, pac_isl, other, mixed, native, minority, edu_coll_plus, unemployment_rate, vacant,
           base = 10,
           offset = 1) %>%
  # create dummy variables plus account for folding issues
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = FALSE) %>%
  # last things are to drop predictors without a lot variation in a sample to avoid errors and to scale/center
  step_nzv(all_predictors()) %>%
  step_normalize(all_predictors()) %>%
  # add splines to variables
  step_ns(white, deg_free = 2) %>%
  step_ns(poverty, deg_free = 4)


# Confirmation: no data issues --------------------------------------------

# don't need to run every time
# prep(hud_recipes[["demo_new"]]) %>%
#   bake(hud_train) %>%
#   View()
# #
# prep(hud_recipes[["all_new"]]) %>%
#    bake(hud_train) %>%
#    View()


# Write out recipes -------------------------------------------------------

hud_recipes_log <- list(all = recipe_all, demo_only = recipe_ppl)
hud_recipes <- list(all_new = recipe_all_new, demo_new = recipe_ppl_new)
save(base_recipe, base_recipe_log, hud_recipes, hud_recipes_log, file = "data/processed/recipes.RData")
