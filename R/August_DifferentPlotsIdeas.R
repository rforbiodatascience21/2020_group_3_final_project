### Basic visualizations

# Loading libraries
library(devtools)
library(datamicroarray)
library(tidyverse)
library(dplyr)

# Loading data
data <- read_tsv("/cloud/project/data/nhgh.tsv")

data_clean <- data %>%
  drop_na()

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

