# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library(tidymodels)
library(tidyverse)
library(workflows)
library(tune)
library(ranger)
library(dplyr)
library(purrr)
library(broom)
library(yardstick)

# Load data ---------------------------------------------------------------
my_data_clean_aug <- read_tsv(file = "data/03_my_data_clean_aug.tsv")

# Data wrangling ----------------------------------------------------------
lr_fit_data <- my_data_clean_aug %>% 
  mutate(Affected_factor = case_when(Affected == "No" ~ 0,
                              Affected == "yes" ~ 1)) 
lr_fit_data <- lr_fit_data %>% mutate_if(is.double, as.factor) %>% 
  mutate_if(is.character, as.factor)

lr_fit_data


glm.fit <- glm(Affected_factor ~ Height + Weight + BMI + Dur_disease + genderBin +
                 Above15 + Betw5_11 + Betw11_15,
               data = lr_fit_data,
               family = binomial)

summary(glm.fit)

glm.probs <- predict(glm.fit,type = "response")
glm.probs


lr_fit_data %>%
  mutate(prob = glm.probs) %>%
  ggplot(aes(Affected_factor, prob)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    title = "Logistic Regression Model", 
    x = "Affected with T1 diabetes",
    y = "Probability of having T1 diabetes"
  )




# Model data --------------------------------------------------------------
lr_fit <- lr_fit %>%
  mutate(mdl = map(data, ~ glm(formula = Affected ~ Height + Weight + BMI + Dur_disease,
                               data = .,
                               family = binomial(link = "logit"))))

log_mod <- logistic_reg() %>%
  set_engine("glm")

log_fit <- log_mod %>%
  fit(Affected ~ Height + Weight + BMI + Dur_disease + genderBin,
      data = lr_fit)
log_fit



set.seed(234589)

# split the data into trainng (75%) and testing (25%)
t1_diabetes_split <- initial_split(lr_fit, 
                                prop = 3/4)

diabetes_train <- training(t1_diabetes_split)
diabetes_test <- testing(t1_diabetes_split)

# create CV object from training data
diabetes_cv <- vfold_cv(diabetes_train)

# define the recipe, we can always add more recipes
diabetes_recipe <- 
  recipe(Affected ~ Height + Weight + BMI + Dur_disease + genderBin + Above15
         + Betw5_11 + Below5, 
         data = lr_fit) %>%
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

# specify which values meant to try
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

final_model <- fit(lr_workflow, lr_fit)

new_example <- tribble~(~Height, ~Weight, ~BMI, ~Dur_disease, ~genderBin, ~Above15,
                       ~Betw5_11, ~Below5,
                       120, 52, 21, 1000, 1, 1, 0, 0)
new_example

predict(final_model, new_data = new_example)

# Visualise data ----------------------------------------------------------
