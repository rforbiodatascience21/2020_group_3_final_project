### Basic visualizations

# Loading libraries
library(devtools)
library(datamicroarray)
library(tidyverse)
library(dplyr)
library(tidymodels)

# Loading data
data <- read_tsv("/cloud/project/data/nhgh.tsv")

# Extracting information from the raw data
dim(data)

# Removing NA's
data_clean <- data %>%
  drop_na()

# Extracting basic information about the data
dim(data_clean)
dim(data_clean %>% filter(sex == "male"))
dim(data_clean %>% filter(sex == "female"))
dim(data_clean %>% filter(wt < 100))
dim(data_clean %>% filter(wt > 100))

# Creating BMI classes

data_clean_bmi <- data_clean %>%
  mutate(BMI_class = case_when(bmi < 18.5 ~ "underweight",
                               18.5 <= bmi & bmi < 25  ~ "normal weight",
                               25 <= bmi & bmi < 30  ~ "overweight",
                               30 <= bmi & bmi < 35 ~ "obese",
                               35 <= bmi & bmi < 40 ~ "severe obesity",
                               40 <= bmi ~ "morbid obesity"))

data_clean_bmi <- data_clean_bmi %>%
  mutate(BMI_class = factor(BMI_class,
                            levels =  c("underweight",
                                        "normal weight",
                                        "overweight",
                                        "obese",
                                        "severe obesity",
                                        "morbid obesity",
                                        "super obese")))

data_clean_bmi <- data_clean_bmi %>%
  mutate(genderBin = case_when(sex == "male" ~ 1,
                               sex == "female" ~ 0))

data_clean_bmi <- data_clean_bmi %>%
  mutate(BFP = round(1.39 * bmi + 0.16 * age -10.34 * genderBin - 9, 1))

# Making boxplot
ggplot(data = data,
       mapping = aes(x = re,
                     y = bmi,
                     fill = sex)) +
  geom_boxplot()

# Scatter plot
ggplot(data = data,
       mapping = aes(x = waist,
                     y = bmi,
                     fill = sex,
                     color = sex)) +
  geom_point()

# Density
ggplot(data = data,
       mapping = aes(x = bmi,
                     fill = re)) +
  geom_density()+
  labs(x="BMI", y="density")

# LM
ggplot(data = data,
       mapping = aes(x = waist,
                     y = bmi,
                     fill = sex,
                     color=sex)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x, se=F) +
  labs(x="waist", y="BMI")

# QQ-plot
ggplot(data = data,
       mapping = aes(sample = waist,
                     colour = factor(re))) +
  stat_qq() +
  stat_qq_line()

# PCA plot

pca_fit <- data %>% 
  select(where(is.numeric)) %>% # retain only numeric columns
  prcomp(scale = TRUE)

pca_fit %>%
  augment(data) %>%
  ggplot(aes(.fittedPC1,
             .fittedPC2,
             color = dx)) + 
  geom_point(size = 1.5) +
  theme_classic() +
  theme(legend.position = "bottom")

pca_fit %>%
  tidy(matrix = "rotation")

arrow_style <- arrow(
  angle = 20, ends = "first", type = "closed", length = grid::unit(8, "pt")
)

pca_fit %>%
  tidy(matrix = "rotation") %>%
  pivot_wider(names_from = "PC",
              names_prefix = "PC",
              values_from = "value") %>%
  ggplot(aes(PC1, PC2)) +
  geom_segment(xend = 0,
               yend = 0,
               arrow = arrow_style) +
  geom_text(
    aes(label = column),
    hjust = 1,
    nudge_x = -0.02, 
    color = "#904C2F"
  ) +
  theme_minimal() +
  coord_fixed() # fix aspect ratio to 1:1

pca_fit %>%
  tidy(matrix = "eigenvalues")

pca_fit %>%
  tidy(matrix = "eigenvalues") %>%
  filter(PC <= 10) %>%
  ggplot(aes(PC,
             percent)) +
  geom_col(fill = "#56B4E9",
           alpha = 0.8) +
  scale_x_continuous(breaks = 1:9) +
  scale_y_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.4)),
    limits = c(0, 0.1)) +
  theme_minimal(base_size = 12,
                base_family = "") 

# K-means
labelled_points <- data_clean_bmi %>%
  select(dx, wt, bmi) %>%
  mutate(x1 = (wt - mean(wt)) / (var(wt) ** (1 / 2)),
         x2 = (bmi - mean(bmi)) / (var(bmi) ** (1 / 2))) %>%
  unnest(cols = c(x1,x2))

ggplot(labelled_points, aes(x1, x2, color = dx)) +
  geom_point(alpha = 0.3)

points <- labelled_points %>% 
  select(-dx)

kclust <- kmeans(points, centers = 2)
kclust
summary(kclust)
augment(kclust, points)

tidy(kclust)

glance(kclust)


kclusts <- 
  tibble(k = 1:9) %>%
  mutate(
    kclust = map(k, ~kmeans(points, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, points)
  )

clusters <- 
  kclusts %>%
  unnest(cols = c(tidied))

assignments <- 
  kclusts %>% 
  unnest(cols = c(augmented))

clusterings <- 
  kclusts %>%
  unnest(cols = c(glanced))

p1 <- 
  ggplot(assignments, aes(x = x1, y = x2)) +
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  facet_wrap(~ k)
p1

p2 <- p1 + geom_point(data = clusters, size = 10, shape = "x")
p2


ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line() +
  geom_point()










