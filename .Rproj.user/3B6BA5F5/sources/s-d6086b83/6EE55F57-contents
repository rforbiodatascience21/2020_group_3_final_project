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
