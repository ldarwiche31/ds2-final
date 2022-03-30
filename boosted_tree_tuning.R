
# Final Project: Boosted Tree ---------------------------------------------
# Name: Leila Darwiche

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

boost_tree_model <-
  boost_tree(min_n = tune(),
             mtry = tune(),
             learn_rate = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

# Tuning parameters -------------------------------------------------------

# Going to use default for the minimum node size and the learn rate but
# want to do a bit more exploring of proportion of predictors to include 50% of predictors


boost_tree_params_demo <- parameters(boost_tree_model) %>%
  update(mtry = mtry(range = c(1, 11)))

boost_tree_params_all <- parameters(boost_tree_model) %>%
  update(mtry = mtry(range = c(1, 21)))

boost_tree_grid_demo <- grid_regular(boost_tree_params_demo, levels = 5)
boost_tree_grid_all <- grid_regular(boost_tree_params_all, levels = 5)

# Workflow ----------------------------------------------------------------

boost_tree_set_all <-
  workflow_set(preproc = hud_recipes["all_new"],
               models = list(boost_tree = boost_tree_model)) %>%
  option_add(param_info = boost_tree_grid_all, id = "all_new_boost_tree")

boost_tree_set_demo <-
  workflow_set(preproc = hud_recipes["demo_new"],
               models = list(boost_tree = boost_tree_model)) %>%
  option_add(param_info = boost_tree_grid_demo, id = "demo_new_boost_tree")

boost_tree_set_all_log <-
  workflow_set(preproc = hud_recipes_log["all"],
               models = list(boost_tree = boost_tree_model)) %>%
  option_add(param_info = boost_tree_grid_all, id = "all_boost_tree")

boost_tree_set_demo_log <-
  workflow_set(preproc = hud_recipes_log["demo_only"],
               models = list(boost_tree = boost_tree_model)) %>%
  option_add(param_info = boost_tree_grid_demo, id = "demo_only_boost_tree")

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

boost_tree_tuned_all <-
  boost_tree_set_all %>%
  workflow_map(
    seed = 274,
    resamples = hud_folds,
    grid = boost_tree_grid_all,
    control = grid_ctrl
  )

boost_tree_tuned_demo <-
  boost_tree_set_demo %>%
  workflow_map(
    seed = 274,
    resamples = hud_folds,
    grid = boost_tree_grid_demo,
    control = grid_ctrl
  )

boost_tree_tuned_all_log <-
  boost_tree_set_all_log %>%
  workflow_map(
    seed = 274,
    resamples = hud_folds,
    grid = boost_tree_grid_all,
    control = grid_ctrl
  )

boost_tree_tuned_demo_log <-
  boost_tree_set_demo_log %>%
  workflow_map(
    seed = 274,
    resamples = hud_folds,
    grid = boost_tree_grid_demo,
    control = grid_ctrl
  )


# Save results ------------------------------------------------------------

save(boost_tree_tuned_all, file = "results/boost_tree_tuned_all.rda")
save(boost_tree_tuned_demo, file = "results/boost_tree_tuned_demo.rda")

save(boost_tree_tuned_all_log, file = "results/boost_tree_tuned_all_log.rda")
save(boost_tree_tuned_demo_log, file = "results/boost_tree_tuned_demo_log.rda")

