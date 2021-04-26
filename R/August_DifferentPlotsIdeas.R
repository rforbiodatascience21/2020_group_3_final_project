### Basic visualizations

# Loading libraries
library(devtools)
library(datamicroarray)
library(tidyverse)
library(dplyr)

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

