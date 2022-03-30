
# Final Project: Test Fit + Analysis --------------------------------------
# Name: Leila Darwiche ----------------------------------------------------

# Load packages
library(tidymodels)
library(tidyverse)
tidymodels_prefer()

# Set seed (will use across all scripts)
set.seed(274)

# Load data
load("data/processed/splits.RData")

# Load model fit
load("results/final_fit.rda")

# Fitting to test set -----------------------------------------------------

final_results <- final_fit %>%
  last_fit(split = split)

collect_metrics(final_results)


training(split) %>% View()
