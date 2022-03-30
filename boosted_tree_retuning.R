
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
  boost_tree(min_n = 11, # best value from initial
             mtry = 16, # best value from initial
             learn_rate = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

# Tuning parameters -------------------------------------------------------

# focusing on learn rate
boost_tree_params <- parameters(boost_tree_model) %>%
  update(learn_rate = learn_rate(range = c(-1, -.25)))

boost_tree_grid <- grid_regular(boost_tree_params, levels = 5)

# Workflow ----------------------------------------------------------------

boost_tree_set <-
  workflow() %>%
  add_recipe(hud_recipes[["all_new"]]) %>%
  add_model(boost_tree_model) 

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

boost_tree_retuned <-
  boost_tree_set %>%
  tune_grid(
    resamples = hud_folds,
    grid = boost_tree_grid,
    control = grid_ctrl
  )

# Save results ------------------------------------------------------------

save(boost_tree_retuned, file = "results/boost_tree_retuned.rda")


