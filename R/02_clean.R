# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("dplyr")
library("tidyr")


# Define functions --------------------------------------------------------
source(file = "R/01_load.R")

# Wrangle data ------------------------------------------------------------
my_data_clean <- my_data %>%
  mutate(Age = case_when(Age == "greater then 15" ~ "> 15",
                         Age == "Less then 11" ~ "< 11",
                         Age == "Less then 15" ~"< 15",
                         Age == "Less then 5" ~ "< 5"),
         HbA1c = case_when(HbA1c == "Over 7.5%" ~"> 7.5%",
                           HbA1c == "Less then 7.5%" ~"< 7.5%"),
         BMI = round(BMI,1),
         Dur_disease = str_extract(`Duration of disease`,"\\d+\\.?\\d*"),
         unit = str_replace(`Duration of disease`, Dur_disease,"")) %>%
  select(-`Duration of disease`)

# Converting duration to days for every value
my_data_clean <- my_data_clean %>%
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


# Write data --------------------------------------------------------------
#write_tsv(x = my_data_clean,
#          file = "data/02_my_data_clean.tsv")

