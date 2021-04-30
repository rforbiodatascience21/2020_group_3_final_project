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
         Dur_disease(days) = str_extract(`Duration of disease`,"\\d+\\.?\\d*"),
         unit = str_replace(`Duration of disease`, Dur_disease(days),"")) %>%
  select(-`Duration of disease`)

# Converting duration to year for every value
my_data_clean <- my_data_clean %>%
  mutate(Dur_disease(days) = as.numeric(Dur_disease(days))) %>%
  mutate(Dur_disease(days) = case_when(unit == "d" ~ Dur_disease(days),
                                 unit == "w" ~ Dur_disease(days) * 7,
                                 unit == "m" ~ Dur_disease(days) * 30,
                                 unit == "y" ~ Dur_disease(days) * 365)) %>%
  select(-unit)




# Write data --------------------------------------------------------------
#write_tsv(x = my_data_clean,
#          file = "data/02_my_data_clean.tsv")
