library("tidyverse")


table <- read.table(file = '/cloud/project/data/nhgh.tsv', sep = '\t', header = TRUE) 


table_clean <- table %>%
  drop_na()

#BMI depending on the income
table_clean <- table_clean %>% 
  mutate(BMI_class = case_when(bmi < 18.5 ~ "Underweight",
                               18.5 <= bmi & bmi < 24.999 ~ "Normal weight",
                               25 <= bmi & bmi < 29.999 ~ "Overweight",
                               30.0 <= bmi & bmi < 34.999 ~ "Obese",
                               35.0 <= bmi & bmi < 39.999 ~ "Severe obesity",
                               40 <= bmi ~ "Morbid obesity"))

table_clean %>%
  ggplot(aes(x = income, fill=factor(BMI_class, levels=c("Underweight","Normal weight", "Overweight","Obese", "Severe obesity", "Morbid obesity")))) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(fill = "Categories")

#BMI Class distribution among diabetic people
table_clean %>%
  filter(dx == "1") %>%
  ggplot(aes(x = income, fill=factor(BMI_class, levels=c("Underweight","Normal weight", "Overweight","Obese", "Severe obesity", "Morbid obesity")))) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(fill = "Categories")

#normal range for the hemoglobin A1c level is between 4% and 5.6%. Hemoglobin A1c levels between 5.7% and 6.4% mean you have a higher chance of getting diabetes. Levels of 6.5% or higher mean you have diabetes.
#Table displaying basic statistics (mean, std, percentiles) 













#machine learning
install.packages("tidymodels")
library(tidymodels)
library(tidyverse)
library(workflows)
library(tune)

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
rf_grid <- expand.grid(mtry = c(3, 4, 5))
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

final_model <- fit(rf_workflow, table_clean)

new_example <- tribble(~age, ~wt, ~gh, ~tri, ~bmi,
                       60.16667, 116.8, 6.0, 29.6, 42.39)
new_example

predict(final_model, new_data = new_example)