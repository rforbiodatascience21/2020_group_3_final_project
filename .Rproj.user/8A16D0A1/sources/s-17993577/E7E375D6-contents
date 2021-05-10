##Libraries Used

library("tidyverse")
library("dplyr")
library("broom")
library("purrr")
library("vroom")

##source(file = "R/99_project_functions.R")

##Loading Data Set
data_raw <- read_tsv(file = "/cloud/project/data/nhgh.tsv")

##Data Tidying
  
  ##1.Remove NA
data <- data_raw  %>% 
  drop_na()
  View(data)

  ##2.Round "age" Integers
data_age_rounded <-data %>%
  mutate(age=round(age, digits=0))
  View(data_age_rounded)

##PCA
  
