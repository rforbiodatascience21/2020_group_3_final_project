# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
my_data <- read_csv2(file = "/cloud/project/data/_raw/untidy_t1_diabetes.csv")

# Write data --------------------------------------------------------------
write_tsv(x = my_data,
          file = "data/01_my_data_load.tsv")