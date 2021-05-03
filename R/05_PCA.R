# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(broom)
library(vroom)
library(cowplot)
library(patchwork)

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
my_data_clean_aug <- read_tsv(file = "data/03_my_data_clean_aug.tsv")

# Data wrangling ----------------------------------------------------------
pca_fit <- my_data_clean_aug %>%
  select(-Affected,
         -AffectedBin) %>%
  select(where(is.numeric)) %>%
  prcomp(scale = TRUE)

# Visualise data ----------------------------------------------------------

# Plotting the variance explained by each component
p1 <- pca_fit %>%
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(PC, percent)) +
  geom_col(fill = "red", alpha = 0.5) +
  scale_x_continuous(breaks = 1:19) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.01))
  ) +
  theme_minimal_hgrid(12) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "PC variance explained",
       x = "PC",
       y = "PERCENT")

# define arrow style for plotting
arrow_style <- arrow(
  angle = 20, ends = "first", type = "closed", length = grid::unit(8, "pt")
)

# Plotting the directions
p2a <- pca_fit %>%
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
    color = "purple"
  ) +
  xlim(-1.25, .5) + ylim(-.5, 1) +
  coord_fixed() + 
  theme_minimal_grid(12)

p2b <- pca_fit %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC",
              names_prefix = "PC",
              values_from = "value") %>%
  ggplot(aes(x = PC1,
             y = PC3)) +
  geom_segment(xend = 0,
               yend = 0,
               arrow = arrow_style) +
  geom_text(
    aes(label = column),
    hjust = 1, nudge_x = -0.02, 
    color = "purple"
  ) +
  xlim(-1.25, .5) + ylim(-.5, 1) +
  coord_fixed() + 
  theme_minimal_grid(12)

p2c <- pca_fit %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC",
              names_prefix = "PC",
              values_from = "value") %>%
  ggplot(aes(x = PC1,
             y = PC4)) +
  geom_segment(xend = 0,
               yend = 0,
               arrow = arrow_style) +
  geom_text(
    aes(label = column),
    hjust = 1, nudge_x = -0.02, 
    color = "purple"
  ) +
  xlim(-1.25, .5) + ylim(-.5, 1) +
  coord_fixed() + 
  theme_minimal_grid(12)

p2d <- pca_fit %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC",
              names_prefix = "PC",
              values_from = "value") %>%
  ggplot(aes(x = PC1,
             y = PC5)) +
  geom_segment(xend = 0,
               yend = 0,
               arrow = arrow_style) +
  geom_text(
    aes(label = column),
    hjust = 1, nudge_x = -0.02, 
    color = "purple"
  ) +
  xlim(-1.25, .5) + ylim(-.5, 1) +
  coord_fixed() + 
  theme_minimal_grid(12)

p2e <- ((p2a + p2b) / (p2c + p2d)) +
  plot_annotation(
    title = "PCA - Directions (T1-Diabetes)") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))
p2e

p3 <- pca_fit %>%
  augment(my_data_clean_aug) %>%
  ggplot(mapping = aes(x = .fittedPC1,
                       y = .fittedPC2,
                       fill = Affected,
                       color = Affected)) +
  geom_point() +
  stat_ellipse(mapping = aes(x = .fittedPC1,
                             y = .fittedPC2,
                             fill = Affected,
                             color = Affected),
               geom="polygon", level=0.95, alpha=0.2) +
  guides(fill = FALSE) + 
  theme(axis.text.x=element_text(angle =0,
                                 vjust=1,
                                 hjust=1),
        legend.position ="none")  +
  labs(x = "PC1",
       y = "PC2")
p3

p4 <- pca_fit %>%
  augment(my_data_clean_aug) %>%
  ggplot(mapping = aes(x = .fittedPC1,
                       y = .fittedPC3,
                       fill = Affected,
                       color = Affected)) +
  geom_point() +
  stat_ellipse(mapping = aes(x = .fittedPC1,
                             y = .fittedPC3,
                             fill = Affected,
                             color = Affected),
               geom="polygon", level=0.95, alpha=0.2) +
  guides(fill = FALSE) +
  theme(axis.text.x=element_text(angle =0, vjust=1, hjust=1),
        legend.position ="none") +
  labs(x = "PC1",
       y = "PC3")

p5 <- pca_fit %>%
  augment(my_data_clean_aug) %>%
  ggplot(mapping = aes(x = .fittedPC1,
                       y = .fittedPC4,
                       fill = Affected,
                       color = Affected)) +
  geom_point() +
  stat_ellipse(mapping = aes(x = .fittedPC1,
                             y = .fittedPC4,
                             fill = Affected,
                             color = Affected),
               geom="polygon", level=0.95, alpha=0.2) +
  guides(fill = FALSE) +
  theme(axis.text.x=element_text(angle =0, vjust=1, hjust=1),
        legend.position ="none") +
  labs(x = "PC1",
       y = "PC4")
p5
p6 <- pca_fit %>%
  augment(my_data_clean_aug) %>%
  ggplot(mapping = aes(x = .fittedPC1,
                       y = .fittedPC5,
                       fill = Affected,
                       color = Affected)) +
  geom_point() +
  stat_ellipse(mapping = aes(x = .fittedPC1,
                             y = .fittedPC5,
                             fill = Affected,
                             color = Affected),
               geom="polygon", level=0.95, alpha=0.2) +
  guides(fill = FALSE) +
  theme(axis.text.x=element_text(angle =0, vjust=1, hjust=1),
        legend.position ="None") +
  labs(x = "PC1",
       y = "PC5")
p6

p7 <- ((p3 + p4) / (p5 + p6)) +
  plot_annotation(
    title = "PCA - Various components (T1-Diabetes)") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))
p7

# Model data
#my_data_clean_aug %>% ...

# Write data --------------------------------------------------------------
#write_tsv(...)
ggsave(plot = p1, filename = "results/05_PCA_varExplained.png")
ggsave(plot = p2e, filename = "results/05_PCA_directions.png")
ggsave(plot = p7, filename = "results/05_PCA_scatter.png")

