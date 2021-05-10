# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(broom)
library(vroom)
library(cowplot)
library(patchwork)

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
  tidy(matrix = "eigenvalues") %>% # getting variance from the the eigenvalues object
  ggplot(mapping = aes(PC,
                       percent)) + # plotting the variance explained by each PC
  geom_col(fill = "red",
           alpha = 0.5) +
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
arrow_style <- arrow(angle = 20,
                     ends = "first",
                     type = "closed",
                     length = grid::unit(4, "pt"))

# Plotting the directions for PC1,(PC2,PC3,PC4)
p2a <- pca_fit %>%
  tidy(matrix = "rotation") %>% # getting the eigenvectors eg. PCs
  pivot_wider(names_from = "PC", # take values from column an create new columns
              values_from = "value", # take value from another column and use in the newly created columns
              names_prefix = "PC") %>% # string added to the start of every variable name
  ggplot(mapping = aes(x = PC1, # draw line between points
                       y = PC2)) +
  geom_segment(xend = 0, # draw line between points
               yend = 0,
               arrow = arrow_style) +
  geom_text(
    aes(label = column),
    hjust = 1, nudge_x = -0.02, 
    color = "purple",
    size = 3) +
  geom_text_repel(aes(label = ""),
                  size =3) +
  xlim(-.6, .6) + ylim(-.5, .5) +
  coord_fixed() + 
  theme_minimal_grid(12)

p2a

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
  geom_text(aes(label = column),
            hjust = 1, nudge_x = -0.02, 
            color = "purple",
            size = 3) +
  xlim(-.6, .6) + ylim(-.5, .5) +
  coord_fixed() + 
  theme_minimal_grid(12)
p2b

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
  geom_text(aes(label = column),
            hjust = 1, nudge_x = -0.02, 
            color = "purple",
            size = 3) +
  xlim(-.6, .6) + ylim(-.5, .5) +
  coord_fixed() + 
  theme_minimal_grid(12)
p2c

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
  geom_text(aes(label = column),
            hjust = 1, nudge_x = -0.02, 
            color = "purple",
            size = 3) +
  xlim(-.6, .6) + 
  ylim(-.5, .5) +
  coord_fixed() + 
  theme_minimal_grid(12)
p2d

p2e <- ((p2a + p2b) / (p2c + p2d)) +
  plot_annotation(title = "PCA - Directions (T1-Diabetes)") +
  plot_layout(guides = "collect") & # moving legend to the bottom 
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
               geom="polygon",
               level=0.95,
               alpha=0.2) +
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
               geom="polygon",
               level=0.95,
               alpha=0.2) +
  guides(fill = FALSE) +
  theme(axis.text.x=element_text(angle =0,
                                 vjust=1,
                                 hjust=1),
        legend.position ="none") +
  labs(x = "PC1",
       y = "PC3")
p4

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
               geom="polygon",
               level=0.95,
               alpha=0.2) +
  guides(fill = FALSE) +
  theme(axis.text.x=element_text(angle =0,
                                 vjust=1,
                                 hjust=1),
        legend.position ="none") +
  labs(x = "PC2",
       y = "PC3")
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
               geom="polygon",
               level=0.95,
               alpha=0.2) +
  guides(fill = FALSE) +
  theme(axis.text.x=element_text(angle =0,
                                 vjust=1,
                                 hjust=1),
        legend.position ="None") +
  labs(x = "PC1",
       y = "PC5")
p6

# Creating one plot with all for subplots. Data projected onto PC1,(PC2,PC3,PC4)
p7 <- ((p3 + p4) / (p5 + p6)) +
  plot_annotation(
    title = "PCA - Various components (T1-Diabetes)") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))
p7


# Write data --------------------------------------------------------------
#write_tsv(...)
ggsave(plot = p1, filename = "results/05_PCA_varExplained.png")
ggsave(plot = p2a, filename = "results/05_PCA_directions_a.png")
ggsave(plot = p2b, filename = "results/05_PCA_directions_b.png")
ggsave(plot = p2c, filename = "results/05_PCA_directions_c.png")
ggsave(plot = p2d, filename = "results/05_PCA_directions_d.png")
ggsave(plot = p2e, filename = "results/05_PCA_directions.png")
ggsave(plot = p7, filename = "results/05_PCA_scatter.png")

