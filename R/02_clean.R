# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


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
         BMI = round(BMI,1))




# Write data --------------------------------------------------------------
write_tsv(x = my_data_clean,
          file = "data/02_my_data_clean.tsv")