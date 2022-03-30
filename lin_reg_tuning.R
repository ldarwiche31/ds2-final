
# Final Project: Penalized Linear Regression ------------------------------
# Name: Leila Darwiche

# Load packages
library(tidymodels)
library(tidyverse)
library(glmnet)
tidymodels_prefer()

# Set seed (will use across all scripts)
set.seed(274)

# Load data
load("data/processed/splits.RData")

# Load recipes
load("data/processed/recipes.RData")


# Model specs -------------------------------------------------------------

lin_reg_model <-
  linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

pen_lin_reg_model <-
  linear_reg(mixture = tune(),
             penalty = tune()) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# Tuning parameters -------------------------------------------------------

# let us just use defaults, tuning more penalty parameters rather than mixture

lin_reg_params <- parameters(pen_lin_reg_model)

lin_reg_grid <- grid_regular(lin_reg_params, levels = c(mixture = 6, penalty = 10))


# Workflow ----------------------------------------------------------------

lin_reg_set <-
  workflow_set(preproc = hud_recipes,
               models = list(pen_lin_reg = pen_lin_reg_model, lin_reg = lin_reg_model)) %>%
  option_add(param_info = lin_reg_grid, id = "all_new_pen_lin_reg") %>%
  option_add(param_info = lin_reg_grid, id = "demo_new_pen_lin_reg")

lin_reg_set_log <-
  workflow_set(preproc = hud_recipes_log,
               models = list(pen_lin_reg = pen_lin_reg_model, lin_reg = lin_reg_model)) %>%
  option_add(param_info = lin_reg_grid, id = "all_pen_lin_reg") %>%
  option_add(param_info = lin_reg_grid, id = "demo_only_pen_lin_reg")

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

# lin_reg_tuned <-
#   lin_reg_set %>%
#   workflow_map(
#     seed = 274,
#     resamples = hud_folds,
#     grid = lin_reg_grid,
#     control = grid_ctrl
#   )

lin_reg_tuned_log <-
  lin_reg_set_log %>%
  workflow_map(
    seed = 274,
    resamples = hud_folds,
    grid = lin_reg_grid,
    control = grid_ctrl
  )


# Save results ------------------------------------------------------------

# save(lin_reg_tuned, file = "results/lin_reg_tuned.rda")
save(lin_reg_tuned_log, file = "results/lin_reg_tuned_log.rda")

lin_reg_tuned %>% collect_metrics() %>% View()
# lin_reg_tuned_log%>% collect_metrics() %>% View()
