# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(broom)
library(vroom)
library(cowplot)
library(patchwork)
library(ggridges)

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
my_data_clean_aug <- read_tsv(file = "data/03_my_data_clean_aug.tsv")

# Data wrangling ----------------------------------------------------------
my_data_clean_aug %>%
  filter(Age == "< 5") %>%
  count()
my_data_clean_aug %>%
  filter(Age == "< 11") %>%
  count()
my_data_clean_aug %>%
  filter(Age == "< 15") %>%
  count()
my_data_clean_aug %>%
  filter(Age == "> 15") %>%
  count()

my_data_clean_aug %>% 
  select(c(starts_with("A")))

# Visualise data ----------------------------------------------------------

# Histogram - Duration of T1 diabetes
p1 <- my_data_clean_aug %>%
  # filtering out individuals that do not have T1 diabetes
  filter(Dur_disease > 0) %>%
  ggplot(mapping = aes(x = Dur_disease,
                       fill = `Area of Residence`,
                       color = `Area of Residence`)) +
  geom_histogram(alpha = 0.5,
                 bins = 25) +
  # repositioning legend to be at the bottom
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of duration of based on residency",
       x = "Duration of T1 diabetes (days)",
       y = "Number of individuals")
p1

# Density plot - Distribution of weights across BMI classes stratified on
# stratified on Age and whether the individuals have T1 diabtes or not
p2 <- ggplot(data = my_data_clean_aug,
             mapping = aes(x = Weight,
                           y = BMI_class,
                           fill = Age)) +
  facet_wrap("Affected") +
  # reordering the legend labels
  scale_fill_viridis_d(labels = c("< 5",
                                  "< 11",
                                  "< 15",
                                  "> 15")) +
  # reordering the y-labels
  scale_y_discrete(limits = rev(c("underweight",
                              "normal weight",
                              "overweight",
                              "obese",
                              "severe obesity",
                              "morbid obesity"))) +
  geom_density_ridges(alpha=0.5) +
  # placing legend at the bottom and centering plot title
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5)) +
  # adding custom labels and title
  labs(title = "Weight distribution across BMI classes stratified on T1 diabetes",
       x = "Weight",
       y = "BMI class")
p2
# Tile plot - Displaying individuals based on having disease and BMI class
# and stratifying on affected by T1 diabetes
p3 <- ggplot(data = my_data_clean_aug,
             mapping = aes(x = `Area of Residence`,
                           y = Age,
                           fill = BMI)) +
  geom_tile(alpha=0.5) +
  # Defining color gradient
  scale_fill_gradient2(low="yellow",
                       mid="orange",
                       high="red",
                       midpoint = .5) +
  # redefining the y-label order
  scale_y_discrete(limits = rev(c("underweight",
                                  "normal weight",
                                  "overweight",
                                  "obese",
                                  "severe obesity",
                                  "morbid obesity"))) +
  # placing legend at the bottom and angling x-labels
  theme(legend.position="bottom",
        axis.text.x = element_text(size = 12,
                                   angle = 45,
                                   hjust = 1),
        axis.text.y = element_text(size = 12)) +
  labs(x="First disease", y="BMI class")
p3

# Model data
#my_data_clean_aug %>% ...

# Write data --------------------------------------------------------------
ggsave(plot = p1, filename = "results/06_ExplorativeAnalysis_Histogram.png")
ggsave(plot = p2, filename = "results/06_ExplorativeAnalysis_Density.png")
ggsave(plot = p3, filename = "results/06_ExplorativeAnalysis_TilePlot.png")



