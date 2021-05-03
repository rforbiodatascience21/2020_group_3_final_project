# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(devtools)
library(dplyr)
library(tidymodels)
library(patchwork)
library(broom)
library(cowplot)

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
my_data_clean_aug <- read_tsv(file = "data/03_my_data_clean_aug.tsv")


# Wrangle data ------------------------------------------------------------

dim(my_data_clean_aug)

my_data_clean_aug %>%
  count(my_data_clean_aug)


# Model data and Data exploration

# We plot a bar chart of duration of type 1 diabetes and

bar_char_dur_disease_female <- my_data_clean_aug %>%
  filter(first_disease != "none" & Sex == "Female") %>%
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

# Simple linear regression duration of disease and BMI
ggplot(data = my_data_clean_aug,
       mapping = aes(x = Dur_disease,
                     y = BMI,
                     color = `Area of Residence`)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x, se=F) +
  labs(x="Duration of type 1 diabetes", y="BMI")

# PCA

# Removing NA's
my_data_clean_aug_dop_na <- my_data_clean_aug %>% drop_na()

pca_fit <- my_data_clean_aug_dop_na %>%
  select(where(is.numeric)) %>% # retain only numeric columns
  prcomp(scale = TRUE)

pca_fit %>% 
  augment(my_data_clean_aug_dop_na) %>%
  ggplot(aes(.fittedPC1, .fittedPC2, color = Hypoglycemis)) + 
  geom_point(size = 1.5) +
  scale_color_manual(
    values = c(Yes = "#D55E00", No = "#0072B2")
  ) +
  theme_half_open(12) + background_grid()

# Looking hard to distinguish based on PC1 and PC2

# Extracting rotation matrix
pca_fit %>%
  tidy(matrix = "rotation")


# define arrow style for plotting
arrow_style <- arrow(
  angle = 20, ends = "first", type = "closed", length = grid::unit(8, "pt")
)

# Plotting rotation matrix
pca_fit %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC",
              names_prefix = "PC",
              values_from = "value") %>%
  ggplot(aes(PC1, PC2)) +
  geom_segment(xend = 0, yend = 0, arrow = arrow_style) +
  geom_text(
    aes(label = column),
    hjust = 1, nudge_x = -0.02, 
    color = "#904C2F"
  ) +
  xlim(-1.25, .5) + ylim(-.5, 1) +
  coord_fixed() + # fix aspect ratio to 1:1
  theme_minimal_grid(12)

# Abbove not working atm

# This works fine
pca_fit %>%
  tidy(matrix = "eigenvalues")

pca_fit %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(PC, percent)) +
  geom_col(fill = "#56B4E9", alpha = 0.8) +
  scale_x_continuous(breaks = 1:9) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.01))
  ) +
  theme_minimal_hgrid(12)


# Visualise data ----------------------------------------------------------
my_data_clean_aug %>% ggplot(
  aes(x = BMI,
      y = Weight )) +
  geom_boxplot()

# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)