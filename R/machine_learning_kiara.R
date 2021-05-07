#i was just curious lol 

library("tidyverse")

table <- read.table(file = '/cloud/project/data/03_my_data_clean_aug.tsv', sep = '\t', header = TRUE) 


#machine learning
#install.packages("tidymodels")
library(tidymodels)
library(tidyverse)
library(workflows)
library(tune)
library(ranger)

table <- table %>%
  drop_na()

set.seed(234589)
# split the data into trainng (75%) and testing (25%)
diabetes_split <- initial_split(table, 
                                prop = 3/4)

diabetes_train <- training(diabetes_split)
diabetes_test <- testing(diabetes_split)

# create CV object from training data
diabetes_cv <- vfold_cv(diabetes_train)

# define the recipe, we can always add more recipes
diabetes_recipe <- 
  recipe(Affected ~ genderBin + Weight + Height + FamHistT1DBin + FamHistT2DBin, 
         data = table) %>%
  # and some pre-processing steps
  step_normalize(all_numeric()) %>%
  step_impute_knn(all_predictors())

diabetes_recipe

diabetes_train_preprocessed <- diabetes_recipe %>%
  # apply the recipe to the training data
  prep(diabetes_train) %>%
  # extract the pre-processed training dataset
  juice()
diabetes_train_preprocessed

rf_model <- 
  # specify that the model is a random forest
  rand_forest() %>%
  # specify that the `mtry` parameter needs to be tuned
  set_args(mtry = tune()) %>%
  # select the engine/package that underlies the model
  set_engine("ranger", importance = "impurity") %>%
  # choose either the continuous regression or binary classification mode
  set_mode("classification") 

rf_workflow <- workflow() %>%
  # add the recipe
  add_recipe(diabetes_recipe) %>%
  # add the model
  add_model(rf_model)

# specify which values eant to try
rf_grid <- expand.grid(mtry = c(2, 3, 4))
# extract results
rf_tune_results <- rf_workflow %>%
  tune_grid(resamples = diabetes_cv, #CV object
            grid = rf_grid, # grid of values to try
            metrics = metric_set(accuracy, roc_auc) # metrics we care about
  )
rf_tune_results %>%
  collect_metrics()

param_final <- rf_tune_results %>%
  select_best(metric = "accuracy")
param_final

rf_workflow <- rf_workflow %>%
  finalize_workflow(param_final)

rf_fit <- rf_workflow %>%
  # fit on the training set and evaluate on test set
  last_fit(diabetes_split)
rf_fit

test_performance <- rf_fit %>% collect_metrics()
test_performance

test_predictions <- rf_fit %>% collect_predictions()
test_predictions

final_model <- fit(rf_workflow, table)

new_example <- tribble(~genderBin, ~Weight, ~Height, ~FamHistT1DBin, ~FamHistT2DBin,
                       0, 58, 1.48, 1, 1)
new_example

predict(final_model, new_data = new_example)