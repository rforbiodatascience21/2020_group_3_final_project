# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
data_raw <- read_tsv(file = "/cloud/project/data/nhgh.tsv")

# Wrangle data ------------------------------------------------------------
data <- data_raw  %>% 
  drop_na()
#View(data)
#data_income <- data %>% pull(income)
#View(data_income)


#typeof(data_income)
#as_factor(data_income)
#View(data_income)

#data_income_levels <- data_income %>% str_replace(data_income, "0", "2")
