# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
#install.packages("tidymodels")
library(tidymodels)
library(tidyverse)
library(workflows)
library(tune)
library(ranger)
library("tidyverse")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
table <- read.table(file = '/cloud/project/data/03_my_data_clean_aug.tsv',
                    sep = '\t',
                    header = TRUE) 

# Data wrangling ----------------------------------------------------------
table <- table %>%
  drop_na()

set.seed(234589)
# Splitting the data into training (75%) and testing (25%) set
diabetes_split <- initial_split(table, 
                                prop = 3/4)

diabetes_train <- training(diabetes_split)
diabetes_test <- testing(diabetes_split)

# create CV (Monte Carlo) object from training data
diabetes_cv <- vfold_cv(diabetes_train)

# define the recipe, we can always add more recipes (we define which columns
# the rf model should use for prediction)
diabetes_recipe <- 
  recipe(Affected ~ genderBin + Weight + Height + FamHistT1DBin + FamHistT2DBin, 
         data = table) %>%
  # Normalizing and imputing data using KNN.
  step_normalize(all_numeric()) %>%
  step_impute_knn(all_predictors())

diabetes_recipe

diabetes_train_preprocessed <- diabetes_recipe %>%
  # Use the recipe on the training data
  prep(diabetes_train) %>%
  # extracting the pre-processed data
  juice()
diabetes_train_preprocessed

# Model data ----------------------------------------------------------

# Defining the RF model
rf_model <- 
  # Random forest model
  rand_forest() %>%
  # mtry parameter to be tuned
  set_args(mtry = tune()) %>%
  # selecting the ranger package which is needed for the rf model
  set_engine("ranger", importance = "impurity") %>%
  # Specifying that we want to do classification
  set_mode("classification") 

# Creating workflow
rf_workflow <- workflow() %>%
  # add the created recipe
  add_recipe(diabetes_recipe) %>%
  # and add the model
  add_model(rf_model)

# Parameter tuning - we specify parameters we want to try
rf_grid <- expand.grid(mtry = c(2, 3, 4, 5, 6, 7, 8, 9, 10))

# Extracting results
rf_tune_results <- rf_workflow %>%
  tune_grid(resamples = diabetes_cv, #CV object
            grid = rf_grid, # grid of values to try
            metrics = metric_set(accuracy, roc_auc) # metrics we care about
  )

# Displaying results
rf_tune_results %>%
  collect_metrics()

# Extracting the best result from the training
param_final <- rf_tune_results %>%
  select_best(metric = "accuracy")
param_final

# We add the optimal parameter to the workflow
rf_workflow <- rf_workflow %>%
  finalize_workflow(param_final)

# Testing the optimal training model on the test set
rf_fit <- rf_workflow %>%
  # fit on the training set and evaluate on test set
  last_fit(diabetes_split)
rf_fit

# Extracting the final performance on the test set
test_performance <- rf_fit %>% collect_metrics()
test_performance

# # Showing the predictions
test_predictions <- rf_fit %>% collect_predictions()
test_predictions

final_model <- fit(rf_workflow, table)

# Saving model
save(final_model , file = 'results/machinelearning.rda')

# Inventing a fake person to see what the model predicts
new_example <- tribble(~genderBin, ~Weight, ~Height, ~FamHistT1DBin, ~FamHistT2DBin,
                       0, 73, 1.65, 0, 0)
new_example

predict(final_model, new_data = new_example)