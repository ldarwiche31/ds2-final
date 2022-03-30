
# Final Project: Random Forest --------------------------------------------
# Name: Leila Darwiche

# since for the other models, the non-logged outcome variable has given less errors
# I am going to save computation power and only run the non-logged outcome recipes

# Load packages
library(tidymodels)
library(tidyverse)
library(xgboost)
tidymodels_prefer()

# Set seed (will use across all scripts)
set.seed(274)

# Load data
load("data/processed/splits.RData")

# Load recipes
load("data/processed/recipes.RData")


# Model specs -------------------------------------------------------------

rf_model <-
  rand_forest(min_n = tune(),
              mtry = tune()) %>% 
  set_engine("ranger") %>%
  set_mode("regression")

# Tuning parameters -------------------------------------------------------

# Going to use default for the minimum node size and the learn rate but
# want to do a bit more exploring of proportion of predictors to include 50% of predictors


rf_params_demo <- parameters(rf_model) %>%
  update(mtry = mtry(range = c(1, 11)))

rf_params_all <- parameters(rf_model) %>%
  update(mtry = mtry(range = c(1, 21)))

rf_grid_demo <- grid_regular(rf_params_demo, levels = 5)
rf_grid_all <- grid_regular(rf_params_all, levels = 5)

# Workflow ----------------------------------------------------------------

rf_set_all <-
  workflow_set(preproc = hud_recipes["all_new"],
               models = list(rf = rf_model)) %>%
  option_add(param_info = rf_grid_all, id = "all_new_rf")

rf_set_demo <-
  workflow_set(preproc = hud_recipes["demo_new"],
               models = list(rf = rf_model)) %>%
  option_add(param_info = rf_grid_demo, id = "demo_new_rf")

# Tuning ------------------------------------------------------------------

# set up parallel processing
parallel::detectCores(logical = TRUE)
doMC::registerDoMC(cores = 4)

grid_ctrl <-
  control_grid(
    save_pred = TRUE,
    parallel_over = "resamples",
    save_workflow = TRUE
  )

rf_tuned_all <-
  rf_set_all %>%
  workflow_map(
    seed = 274,
    resamples = hud_folds,
    grid = rf_grid_all,
    control = grid_ctrl
  )
print("first done")
rf_tuned_demo <-
  rf_set_demo %>%
  workflow_map(
    seed = 274,
    resamples = hud_folds,
    grid = rf_grid_demo,
    control = grid_ctrl
  )
print("second done")


# Save results ------------------------------------------------------------

save(rf_tuned_all, file = "results/rf_tuned_all.rda")
save(rf_tuned_demo, file = "results/rf_tuned_demo.rda")
