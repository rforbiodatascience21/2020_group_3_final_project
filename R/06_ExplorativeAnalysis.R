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

# Histogram
p1 <- my_data_clean_aug %>%
  filter(Dur_disease > 0) %>%
  ggplot(mapping = aes(x = Dur_disease,
                       fill = `Area of Residence`,
                       color = `Area of Residence`)) +
  geom_histogram(alpha = 0.5,
                 bins = 25) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of BMI based on M/F")
p1

# Density plot
p2 <- ggplot(data = my_data_clean_aug,
             mapping = aes(x = Weight,
                           y = BMI_class,
                           fill = Age)) +
  facet_wrap("Affected") +
  scale_fill_viridis_d(labels = c("< 5",
                                  "< 11",
                                  "< 15",
                                  "> 15")) +
  scale_y_discrete(limits = rev(c("underweight",
                              "normal weight",
                              "overweight",
                              "obese",
                              "severe obesity",
                              "morbid obesity"))) +
  geom_density_ridges(alpha=0.5) +
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Weight distribution across BMI classes stratified on T1 diabetes",
       x = "Height",
       y = "BMI class")
p2
# Tile plot
p3 <- ggplot(data = my_data_clean_aug,
             mapping = aes(x = first_disease,
                           y = BMI_class,
                           fill = AffectedBin)) +
  geom_tile(alpha=0.5) +
  scale_fill_gradient2(low="yellow",
                       mid="orange",
                       high="red",
                       midpoint = .5) +
  scale_y_discrete(limits = rev(c("underweight",
                                  "normal weight",
                                  "overweight",
                                  "obese",
                                  "severe obesity",
                                  "morbid obesity"))) +
  theme(legend.position="bottom",
        axis.text.x = element_text(size = 12,
                                   angle = 45,
                                   hjust = 1),
        axis.text.y = element_text(size = 12))+
  labs(x="First disease", y="BMI class")
p3

# Model data
#my_data_clean_aug %>% ...

# Write data --------------------------------------------------------------
#write_tsv(...)
#ggsave(...)

