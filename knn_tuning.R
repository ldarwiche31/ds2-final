# Final Project: K-Nearest Neighbors --------------------------------------
# Name: Leila Darwiche

# Load packages
library(tidymodels)
library(tidyverse)
tidymodels_prefer()

# Set seed (will use across all scripts)
set.seed(274)

# Load data
load("data/processed/splits.RData")

# Load recipes
load("data/processed/recipes.RData")


# Model specs -------------------------------------------------------------

knn_model <-
  nearest_neighbor(neighbors = tune()) %>%
  set_engine("kknn") %>%
  set_mode("regression")

# Tuning parameters -------------------------------------------------------

# for regression, the prediction is given from averaging close values... realistically I think we can imagine counties
# having very diff values among diff dimensionalities... we maybe may for this reason choose to tune anywhere from 
# 5 to 50... lets choose 6 levels for now as we can always tune again with smaller range once we narrow down
# (gives us exactly 9 in between each)

knn_params <- parameters(knn_model) %>%
  update(neighbors = neighbors(range = c(5, 50)))

knn_grid <- grid_regular(knn_params, levels = 6)


# Workflow ----------------------------------------------------------------

# original recipes
knn_set_log <-
  workflow_set(preproc = hud_recipes_log,
               models = list("knn" = knn_model)) %>%
  option_add(param_info = knn_grid, id = "all_knn") %>%
  option_add(param_info = knn_grid, id = "demo_only_knn")

# not logged outcome recipes
knn_set <-
  workflow_set(preproc = hud_recipes,
               models = list("knn" = knn_model)) %>%
  option_add(param_info = knn_grid, id = "all_new_knn") %>%
  option_add(param_info = knn_grid, id = "demo_new_knn")

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

knn_tuned_log <-
  knn_set_log %>%
  workflow_map(
    seed = 274,
    resamples = hud_folds,
    grid = knn_grid,
    control = grid_ctrl
  )

knn_tuned <-
  knn_set %>%
  workflow_map(
    seed = 274,
    resamples = hud_folds,
    grid = knn_grid,
    control = grid_ctrl
  )


# Save results ------------------------------------------------------------

save(knn_tuned_log, file = "results/knn_tuned_log.rda")
save(knn_tuned, file = "results/knn_tuned.rda")

load("results/knn_tuned_log.rda")
load("results/knn_tuned.rda")

knn_tuned_log %>% collect_metrics() %>% View()
knn_tuned %>% collect_metrics() %>% View()
