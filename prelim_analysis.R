
# Final Project: Initial Analysis -----------------------------------------
# Name: Leila Darwiche ----------------------------------------------------

# Load packages
library(tidymodels)
library(tidyverse)
tidymodels_prefer()

# Set seed (will use across all scripts)
set.seed(274)

# Load data
load("data/processed/splits.RData")
hud_train <- training(split)

# Load models tuning results
files <- dir("results/", pattern = "\\.rda$", full.names = TRUE)

for (i in seq_along(files)) {
  load(files[[i]])
}


# EXPLORE ALL MODELS ------------------------------------------------------

# quick exploration... can see that non log gave us less errors... so lets ignore logged outcome ones

# Boosted Tree ------------------------------------------------------------

ggplot(boost_tree_tuned_demo %>% collect_metrics() %>% filter(.metric == "rmse"),
       aes(x = mtry, y = mean, color = as.factor(learn_rate))) +
  geom_point() +
  geom_line() +
  facet_wrap(~min_n)

ggplot(boost_tree_tuned_all %>% collect_metrics() %>% filter(.metric == "rmse"),
       aes(x = mtry, y = mean, color = as.factor(learn_rate))) +
  geom_point() +
  geom_line() +
  facet_wrap(~min_n)

collect_metrics(boost_tree_tuned_demo) %>%
  union_all(collect_metrics(boost_tree_tuned_all)) %>%
  filter(.metric == "rmse") %>%
  arrange(mean) %>% View()

# The learning rate seemed to matter most... we might want to rerun the all recipe using some higher learning rates


# K-Nearest Neighbors -----------------------------------------------------

ggplot(knn_tuned %>% collect_metrics() %>% filter(.metric == "rmse"),
       aes(x = neighbors, y = mean, color = wflow_id)) +
  geom_point() +
  geom_line()

rank_results(knn_tuned, select_best = TRUE)


# Linear Models (Penalized and Regular) -----------------------------------

collect_metrics(lin_reg_tuned) %>%
  filter(.metric == "rmse") %>%
  arrange(mean) %>% View()

# The regular linear regression performed nearly as well as the others!

ggplot(lin_reg_tuned %>% collect_metrics() %>% filter(.metric == "rmse"),
       aes(x = penalty, y = mean, color = as.factor(mixture))) +
  geom_point() +
  geom_line() +
  facet_wrap(~wflow_id)


# Random Forest -----------------------------------------------------------

ggplot(rf_tuned_demo %>% collect_metrics() %>% filter(.metric == "rmse"),
       aes(x = mtry, y = mean, color = as.factor(min_n))) +
  geom_point() +
  geom_line() 

ggplot(rf_tuned_all %>% collect_metrics() %>% filter(.metric == "rmse"),
       aes(x = mtry, y = mean, color = as.factor(min_n))) +
  geom_point() +
  geom_line()

collect_metrics(rf_tuned_all) %>%
  union_all(collect_metrics(rf_tuned_demo)) %>%
  filter(.metric == "rmse") %>%
  arrange(mean) %>% View()


# Combining them all ------------------------------------------------------

collect_metrics(rf_tuned_all) %>%
  union_all(collect_metrics(rf_tuned_demo)) %>%
  union_all(collect_metrics(lin_reg_tuned)) %>%
  union_all(collect_metrics(knn_tuned)) %>%
  union_all(collect_metrics(boost_tree_tuned_demo)) %>%
  union_all(collect_metrics(boost_tree_tuned_all)) %>%
  filter(.metric == "rmse") %>%
  arrange(mean) %>% View()

# rf performed best wary to use min-n = 2... feels like too much diff and overfitting...
# and also am feeling like for min_n = 11 and mtry = 11, pay off for more mtry is so marginal
# inclined to use that model, still within the same pay off SE as the others


# Quick Retuning ----------------------------------------------------------

# The boosted tree really seemed to need larger learn rates... so I would like to try doing it over
# a different and larger range... I only need to use one recipe
# look at file boost_tree_retuning.R

collect_metrics(rf_tuned_all) %>%
  union_all(collect_metrics(rf_tuned_demo)) %>%
  union_all(collect_metrics(lin_reg_tuned)) %>%
  union_all(collect_metrics(knn_tuned)) %>%
  union_all(collect_metrics(boost_tree_retuned) %>%
              mutate(wflow_id = "all_new_boost_tree",
                     model = "boost_tree",
                     mtry = 16,
                     min_n = 11)) %>%
  union_all(collect_metrics(boost_tree_tuned_all)) %>%
  filter(.metric == "rmse") %>%
  select(wflow_id, model, .metric, mean, std_err, mtry, min_n, learn_rate, penalty, mixture, neighbors) %>%
  arrange(mean) %>% View()

collect_metrics(rf_tuned_all) %>%
  union_all(collect_metrics(rf_tuned_demo)) %>%
  union_all(collect_metrics(lin_reg_tuned)) %>%
  union_all(collect_metrics(knn_tuned)) %>%
  union_all(collect_metrics(boost_tree_retuned) %>%
              mutate(wflow_id = "all_new_boost_tree",
                     model = "boost_tree",
                     mtry = 16,
                     min_n = 11)) %>%
  union_all(collect_metrics(boost_tree_tuned_all)) %>%
  filter(.metric == "rmse") %>%
  select(wflow_id, model, .metric, mean, std_err, mtry, min_n, learn_rate, penalty, mixture, neighbors) %>%
  arrange(mean) %>% View()

# For comparison, we can look at the sd of the training set as part of the benchmark
# (the NULL model... (i.e. just estimating the mean) standard error)

sd(hud_train$total_units_per_1000)

# How much does rmse decrease with our model?

(7.767562 - sd(hud_train$total_units_per_1000))/sd(hud_train$total_units_per_1000)

# It decreases by 20% which isn't a huge amount but is a decent decrease

# Fit model to all of training --------------------------------------------

final_model <- rf_tuned_all %>%
  extract_workflow_set_result("all_new_rf") %>%
  tune::show_best(n = 10) %>%
  filter(min_n == 11, mtry == 11)

final_fit <- 
  rf_tuned_all %>% 
  extract_workflow("all_new_rf") %>% 
  finalize_workflow(final_model)

save(final_fit, file = "results/final_fit.rda")
