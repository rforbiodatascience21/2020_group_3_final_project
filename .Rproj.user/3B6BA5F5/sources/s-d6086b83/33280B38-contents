# lr model

library("tidyverse")

table <- read.table(file = '/cloud/project/data/nhgh.tsv', sep = '\t', header = TRUE) 


table_clean <- table %>%
  drop_na()

#machine learning
#install.packages("tidymodels")
library(tidymodels)
library(tidyverse)
library(workflows)
library(tune)
library(ranger)

#instead of 0 and 1 we put factors
table_clean <- table_clean %>% 
  mutate(diabetes = case_when(dx == 0 ~ "not diabetes",
                              dx == 1 ~ "diabetes"
  ))

set.seed(234589)
# split the data into trainng (75%) and testing (25%)
diabetes_split <- initial_split(table_clean, 
                                prop = 3/4)

diabetes_train <- training(diabetes_split)
diabetes_test <- testing(diabetes_split)

# create CV object from training data
diabetes_cv <- vfold_cv(diabetes_train)

# define the recipe, we can always add more recipes
diabetes_recipe <- 
  recipe(diabetes ~ age + wt + gh + tri + bmi, 
         data = table_clean) %>%
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

lr_model <- 
  # specify that the model is a random forest
  logistic_reg() %>%
  # select the engine/package that underlies the model
  set_engine("glm") %>%
  # choose either the continuous regression or binary classification mode
  set_mode("classification")

lr_workflow <- workflow() %>%
  # add the recipe
  add_recipe(diabetes_recipe) %>%
  # add the model
  add_model(lr_model)

# specify which values eant to try
lr_grid <- expand.grid(mtry = c(3, 4, 5))
# extract results
lr_tune_results <- lr_workflow %>%
  tune_grid(resamples = diabetes_cv, #CV object
            grid = lr_grid, # grid of values to try
            metrics = metric_set(accuracy, roc_auc) # metrics we care about
  )
lr_tune_results %>%
  collect_metrics()

param_final <- lr_tune_results %>%
  select_best(metric = "accuracy")
param_final

lr_workflow <- lr_workflow %>%
  finalize_workflow(param_final)

lr_fit <- lr_workflow %>%
  # fit on the training set and evaluate on test set
  last_fit(diabetes_split)
lr_fit

test_performance <- lr_fit %>% collect_metrics()
test_performance

test_predictions <- lr_fit %>% collect_predictions()
test_predictions

final_model <- fit(lr_workflow, table_clean)

new_example <- tribble(~age, ~wt, ~gh, ~tri, ~bmi,
                       60.16667, 116.8, 6.0, 29.6, 42.39)
new_example

predict(final_model, new_data = new_example)