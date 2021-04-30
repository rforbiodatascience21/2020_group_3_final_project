# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
my_data_clean <- read_tsv(file = "data/02_my_data_clean.tsv")


# Wrangle data ------------------------------------------------------------
my_data_clean_aug <- my_data_clean %>%
  Dur_disease = str_extract(`Duration of disease`,"\\d+\\.?\\d*"),
  unit = str_replace(`Duration of disease`, Dur_disease,"")) %>%
  select(-`Duration of disease`)

# Converting duration to days for every value
my_data_clean_aug <- my_data_clean %>%
  mutate(Dur_disease = as.numeric(Dur_disease)) %>%
  mutate(Dur_disease = case_when(unit == "d" ~ Dur_disease,
                                 unit == "w" ~ Dur_disease * 7,
                                 unit == "m" ~ Dur_disease * 30,
                                 unit == "y" ~ Dur_disease * 365)) %>%
  separate(`Other diease`,
           into = c("first_disease",
                    "second_disease",
                    "third_disease"),
           sep = ",") %>%
  mutate(first_disease = case_when(first_disease == "no" ~ "none",
                                   first_disease != "no" ~ first_disease),
         second_disease = replace_na(second_disease, "none"),
         third_disease = replace_na(third_disease, "none")) %>%
  select(-unit)

Tibble1 <- my_data_clean_aug %>%
  select(BMI,`Insulin taken`)
Tibble2 <- my_data_clean_aug %>%
  select(BMI, `Impaired glucose metabolism`)

DemonstratingJoin <- full_join(x = Tibble1,
                               y = Tibble2,
                               by = "BMI")

bmi <- my_data_clean_aug %>%
  select(BMI)

disease_BMI <- full_join(x = diseases,
                         y = bmi)

# Write data --------------------------------------------------------------
write_tsv(x = my_data_clean_aug,
          file = "data/03_my_data_clean_aug.tsv")