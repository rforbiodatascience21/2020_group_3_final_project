## ----setup, include=FALSE--------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---- echo=FALSE, out.width = '100%'---------------------------------------------
knitr::include_graphics("/cloud/project/data/_raw/data_science_cycle.png")


## ---- echo=FALSE, out.width = '100%'---------------------------------------------
knitr::include_graphics("/cloud/project/data/_raw/rawdata.png")


## ---- message=FALSE, out.width = '100%'------------------------------------------
# Load libraries
library("tidyverse")
# Load data
my_data_clean <- read_tsv(file = "/cloud/project/data/02_my_data_clean.tsv")
#mutate column 
my_data_clean <- my_data_clean %>%
  mutate(Age = case_when(Age == "greater then 15" ~ "> 15",
                         Age == "Less then 11" ~ "< 11",
                         Age == "Less then 15" ~"< 15",
                         Age == "Less then 5" ~ "< 5"),
         HbA1c = case_when(HbA1c == "Over 7.5%" ~"> 7.5%",
                           HbA1c == "Less then 7.5%" ~"< 7.5%"),
         BMI = round(BMI, 1))



## ---- echo=TRUE, out.width = '200%'----------------------------------------------
# Wrangle data 
my_data_clean_aug <- my_data_clean %>%
  mutate(Dur_disease = str_extract(`Duration of disease`,"\\d+\\.?\\d*"),
  unit = str_replace(`Duration of disease`, Dur_disease,"")) %>%
  select(-`Duration of disease`)


## ----warning=FALSE, out.width = '200%'-------------------------------------------
# Converting duration to days for every value
my_data_clean_aug <- my_data_clean_aug %>%
  mutate(Dur_disease = as.numeric(Dur_disease)) %>%
  mutate(Dur_disease = case_when(unit == "d" ~ Dur_disease,
                                 unit == "w" ~ Dur_disease * 7,
                                 unit == "m" ~ Dur_disease * 30,
                                 unit == "y" ~ Dur_disease * 365),
         Dur_disease = replace_na(Dur_disease, 0)) %>%
  
  # We do not need the unit column anymore
  select(-unit) %>%
  
  # Separating "Other disease" column into three
  separate(`Other diease`,
           into = c("first_disease",
                    "second_disease",
                    "third_disease"),
           sep = ",")


## ---- echo=FALSE, out.width = '100%'---------------------------------------------
knitr::include_graphics("/cloud/project/results/Scatter1.png")


## ---- echo=FALSE, out.width = '100%'---------------------------------------------
knitr::include_graphics("/cloud/project/results/06_ExplorativeAnalysis_Histogram.png")


## ---- echo=FALSE, out.width = '110%'---------------------------------------------
knitr::include_graphics("/cloud/project/results/ScatterPatch.png")


## ---- echo=FALSE, out.width = '100%'---------------------------------------------
knitr::include_graphics("/cloud/project/results/06_ExplorativeAnalysis_Density.png")


## ----pressure, echo=FALSE, out.width = '100%'------------------------------------
knitr::include_graphics("/cloud/project/results/05_PCA_varExplained.png")


## ---- echo=FALSE, fig.cap="Data is well seperated so classification seems to be feasible.", out.width = '100%'----
knitr::include_graphics("/cloud/project/results/05_PCA_scatter.png")


## --------------------------------------------------------------------------------



## ---- echo=FALSE, out.width = '100%'---------------------------------------------
knitr::include_graphics("/cloud/project/results/07_densityPredictions.png")


## ---- echo=FALSE, out.width = '65%'----------------------------------------------

knitr::include_graphics("/cloud/project/results/shiny.png")

