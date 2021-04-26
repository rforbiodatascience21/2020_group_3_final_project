### Basic visualizations

# Loading libraries
library(devtools)
library(datamicroarray)
library(tidyverse)
library(dplyr)

# Loading data
data("chiaretti", package = "datamicroarray")

# Extracting variables
x <- chiaretti %>%
  pluck("x") %>%
  as_tibble()

# Extracting target values
y <- chiaretti %>%
  pluck("y") %>%
  as_tibble() %>%

complete_data <- bind_cols(x,y)

# Making boxplot
ggplot(data = complete_data,
       mapping = aes(x = value,
                     y = `1000_at`,
                     fill = value)) +
  geom_boxplot()

ggplot(data = complete_data,
       mapping = aes(x = `1000_at`,
                     y = `1001_at`,
                     fill = value,
                     color = value)) +
  geom_point()

ggplot(data = complete_data,
       mapping = aes(x = `1002_f_at`,
                     fill = value)) +
  geom_density()+
  labs(x="NH4", y="density")

ggplot(data = complete_data,
       mapping = aes(x = `1000_at`,
                     y = `1001_at`,
                     color=value)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x, se=F) +
  labs(x="CODt", y="CODs")

