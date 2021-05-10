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
lr_fit_data <- lr_fit_data %>% mutate_if(is.double, as.numeric) %>% select(where(is.numeric))

View(lr_fit_data)


ggplot(data = lr_fit_data,
       mapping = aes(x = Dur_disease,
                     y = BMI,
                     color = `Area of Residence`)) +
  geom_point() +
  geom_smooth(method='glm', formula= y~x, se=F) +
  labs(x="Duration of type 1 diabetes", y="BMI")

glm.fit <- glm(Affected_factor ~ Height + Weight + BMI + Dur_disease + genderBin +
                 Above15 + Betw5_11 + Betw11_15 + Below5 + AdeqNutrBin + EducMotherBin +
                 AutoAB_bin + ImpairedGMBin + InsulinTakenBin + FamHistT1DBin + FamHistT2DBin +
                 HypoglycemisBin,
               data = lr_fit_data,
               family = binomial)

summary(glm.fit)

# No significant parameters ...
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