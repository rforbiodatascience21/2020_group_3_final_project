# Data visualization

# Loading libraries
library(readr)

# Loading data and count different features
leukemia_data <- read_csv("/cloud/project/data/chiaretti.csv")

dim(leukemia_data)

leukemia_data %>%
  count(value)


# Plots

ggplot(data = leukemia_data,
       aes(x = value,
           y = `1001_at`)) +
  geom_boxplot()
  