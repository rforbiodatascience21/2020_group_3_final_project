# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(devtools)
library(dplyr)
library(tidymodels)
library(patchwork)

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
my_data_clean_aug <- read_tsv(file = "data/03_my_data_clean_aug.tsv")


# Wrangle data ------------------------------------------------------------

dim(my_data_clean_aug)

my_data_clean_aug %>%
  count(my_data_clean_aug)


# Model data
x <- read_tsv(file = "data/03_my_data_clean_aug.tsv")
View(x)
numeric_data <- my_data_clean_aug %>% select(BMI, Weight,Height, Dur_disease) %>%
  drop_na()

dim(numeric_data)
View(my_data_clean_aug)
# Data exploration

# We plot a bar chart of duration of type 1 diabetes and

bar_char_dur_disease_female <- my_data_clean_aug %>%
  filter(first_disease != "none" & Sex == "Female") %>%
  mutate(disease = factor(first_disease), 
         state = fct_reorder(first_disease,
                             Dur_disease) %>%
           fct_rev()) %>% 
  ggplot(mapping = aes(x = first_disease,
                       y = Dur_disease,
                       fill = `Area of Residence`)) + 
  geom_bar(stat = "identity") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4)) +
  labs(y = "Duration of disease in days",
       x = "First Disease",
       title = "Duration of disease for each first disease for female")

bar_char_dur_disease_male <- my_data_clean_aug %>%
  filter(first_disease != "none" & Sex == "Male") %>%
  mutate(disease = factor(first_disease), 
         state = fct_reorder(first_disease,
                             Dur_disease) %>%
           fct_rev()) %>% 
  ggplot(mapping = aes(x = first_disease,
                       y = Dur_disease,
                       fill = `Area of Residence`)) + 
  geom_bar(stat = "identity") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4)) +
  labs(y = "Duration of disease in days",
       x = "First Disease",
       title = "Duration of disease for each first disease for male")

# Using patchwork to show both plots.
bar_char_dur_disease_female/bar_char_dur_disease_male


#my_data_clean_aug %>% 
#  ggplot(mapping = aes(x = Dur_disease
#                        y = Dur_disease,
#                       fill = `Area of Residence`))
    
#    aes(urbanpop, rate, color = crime)) + 
#  facet_wrap(~crime, scales = "free_y", ncol = 1) +
#  geom_point() + 
#  geom_smooth(se = FALSE, method = "lm") +
#  theme_bw() + 
#  labs(x = "Percentage Urban Population",
#       y = "Arrest Rate per 100,000 people",
#       title = "Arrest rate vs percentage urban population") +
#  theme(legend.title = element_blank(),
#        legend.position = "bottom")

# Visualise data ----------------------------------------------------------
my_data_clean_aug %>% ggplot(
  aes(x = BMI,
      y = Weight )) +
  geom_boxplot()

# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)