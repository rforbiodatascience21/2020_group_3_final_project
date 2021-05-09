# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Load data ---------------------------------------------------------------
my_data_clean <- read_tsv(file = "data/02_my_data_clean.tsv")


# Wrangle data ------------------------------------------------------------
my_data_clean_aug <- my_data_clean %>%
  mutate(Dur_disease = str_extract(`Duration of disease`,
                                   "\\d+\\.?\\d*"),
  unit = str_replace(`Duration of disease`,
                     Dur_disease,
                     "")) %>%
  select(-`Duration of disease`)

# Converting duration to days for every value
my_data_clean_aug <- my_data_clean_aug %>%
  mutate(Dur_disease = as.numeric(Dur_disease)) %>%
  mutate(Dur_disease = case_when(unit == "d" ~ Dur_disease,
                                 unit == "w" ~ Dur_disease * 7,
                                 unit == "m" ~ Dur_disease * 30,
                                 unit == "y" ~ Dur_disease * 365),
         Dur_disease = replace_na(Dur_disease,
                                  0)) %>%
  # We do not need the unit column anymore
  select(-unit) %>%
  # Separating "Other diease" column into three
  separate(`Other diease`,
           into = c("first_disease",
                    "second_disease",
                    "third_disease"),
           sep = ",") %>%                 # Converting no to "none and removing excess space 
  mutate(first_disease = case_when(first_disease == "no" ~ "none",
                                   first_disease != "no" ~ str_squish(str_to_lower(first_disease))),
         second_disease = str_squish(replace_na(second_disease, "none")),
         third_disease = str_squish(replace_na(third_disease, "none"))) %>%
  # We binarize some of the variables and group BMI
  mutate(genderBin = factor(case_when(Sex == "Male" ~ 0,
                               Sex == "Female" ~ 1)),
         Above15 = factor(case_when(Age == "> 15" ~ 1,
                            Age != "> 15" ~ 0)),
         Betw5_11 = factor(case_when(Age == "< 11" ~ 1,
                             Age != "< 11" ~ 0)),
         Betw11_15 = factor(case_when(Age == "< 15" ~ 1,
                               Age != "< 15" ~ 0)),
         Below5 = factor(case_when(Age == "< 5" ~ 1,
                            Age != "< 5" ~ 0)),
         AdeqNutrBin = factor(case_when(`Adequate Nutrition` == "Yes" ~ 1,
                                 `Adequate Nutrition` == "No" ~ 0)),
         EducMotherBin = factor(case_when(`Education of Mother` == "Yes" ~ 1,
                                   `Education of Mother` == "No" ~ 0)),
         AutoAB_bin = factor(case_when(Autoantibodies == "Yes" ~ 1,
                                Autoantibodies == "No" ~ 0)),
         ImpairedGMBin = factor(case_when(`Impaired glucose metabolism` == "Yes" ~ 1,
                                   `Impaired glucose metabolism` == "No" ~ 0)),
         InsulinTakenBin = factor(case_when(`Insulin taken` == "Yes" ~ 1,
                                     `Insulin taken` == "No" ~ 0)),
         FamHistT1DBin = factor(case_when(`Family History affected in Type 1 Diabetes` == "Yes" ~ 1,
                                   `Family History affected in Type 1 Diabetes` == "No" ~ 0)),
         FamHistT2DBin = factor(case_when(`Family History affected in Type 2 Diabetes` == "Yes" ~ 1,
                                   `Family History affected in Type 2 Diabetes` == "No" ~ 0)),
         HypoglycemisBin = factor(case_when(Hypoglycemis == "Yes" ~ 1,
                                     Hypoglycemis == "No" ~ 0)),
         PancreasBin = factor(case_when(`pancreatic disease affected in child` == "Yes" ~ 1,
                                 `pancreatic disease affected in child` == "No" ~ 0)),
         AffectedBin = factor(case_when(Affected == "yes" ~ 1,
                                 Affected == "No" ~ 0)),
         HbA1cBin = factor(case_when(HbA1c == "> 7.5%" ~ 1,
                              HbA1c == "< 7.5%" ~ 0)),
         other_disease_binary = case_when(first_disease == "none" ~ 0,
                                          first_disease != "none" ~ 1,
                                          second_disease == "none" ~ 0,
                                          second_disease != "none" ~ 1,
                                          third_disease == "none" ~ 0,
                                          third_disease != "none" ~ 1,),
         BMI_class = case_when(BMI < 18.5 ~ "underweight",
                               18.5 <= BMI & BMI < 25  ~ "normal weight",
                               25 <= BMI & BMI < 30  ~ "overweight",
                               30 <= BMI & BMI < 35 ~ "obese",
                               35 <= BMI & BMI < 40 ~ "severe obesity",
                               40 <= BMI ~ "morbid obesity"))

# This is not necessary but demonstrating the use of joining
Tibble1 <- my_data_clean_aug %>%
  select(BMI,`Insulin taken`)
Tibble2 <- my_data_clean_aug %>%
  select(BMI, `Impaired glucose metabolism`)

DemonstratingJoin <- full_join(x = Tibble1,
                               y = Tibble2,
                               by = "BMI")

# Write data --------------------------------------------------------------
write_tsv(x = my_data_clean_aug,
          file = "data/03_my_data_clean_aug.tsv")