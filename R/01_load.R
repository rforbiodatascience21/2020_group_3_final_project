# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
my_data_raw <- read_tsv(file = "/cloud/project/data/nhgh.tsv")
View(my_data_raw)

# Wrangle data ------------------------------------------------------------
my_data <- my_data_raw  %>% 
  drop_na()
