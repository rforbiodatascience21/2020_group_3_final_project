# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(broom)
library(vroom)
library(cowplot)

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
my_data_clean_aug <- read_tsv(file = "data/03_my_data_clean_aug.tsv")

# Data wrangling ---------------------------------------------------------------

# Visualise data ----------------------------------------------------------
# PC analysis
pca_fit <- my_data_clean_aug %>%
  select(-Affected,
         -AffectedBin) %>%
  select(where(is.numeric)) %>%
  prcomp(scale = TRUE)

# Plotting the variance explained by each component
pca_fit %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(PC, percent)) +
  geom_col(fill = "#56B4E9", alpha = 0.8) +
  scale_x_continuous(breaks = 1:19) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.01))
  ) +
  theme_minimal_hgrid(12)

# define arrow style for plotting
arrow_style <- arrow(
  angle = 20, ends = "first", type = "closed", length = grid::unit(8, "pt")
)

# Plotting the directions
pca_fit %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC",
              names_prefix = "PC",
              values_from = "value") %>%
  ggplot(aes(x = PC1,
             y = PC2)) +
  geom_segment(xend = 0,
               yend = 0,
               arrow = arrow_style) +
  geom_text(
    aes(label = column),
    hjust = 1, nudge_x = -0.02, 
    color = "#904C2F"
  ) +
  xlim(-1.25, .5) + ylim(-.5, 1) +
  coord_fixed() + 
  theme_minimal_grid(12)

pca_fit %>%
  augment(my_data_clean_aug) %>%
  ggplot(mapping = aes(x = .fittedPC2,
                       y = .fittedPC7,
                       fill = Affected,
                       color = Affected)) +
  geom_point()

# Model data
#my_data_clean_aug %>% ...

# Write data --------------------------------------------------------------
#write_tsv(...)
#ggsave(...)