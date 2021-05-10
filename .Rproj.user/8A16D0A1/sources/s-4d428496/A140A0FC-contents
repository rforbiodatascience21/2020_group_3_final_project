library("tidyverse")


table <- read.table(file = '/cloud/project/data/nhgh.tsv', sep = '\t', header = TRUE) 


table_clean <- table %>%
  drop_na()

#BMI depending on the income
table_clean <- table_clean %>% 
  mutate(BMI_class = case_when(bmi < 18.5 ~ "Underweight",
                               18.5 <= bmi & bmi < 24.999 ~ "Normal weight",
                               25 <= bmi & bmi < 29.999 ~ "Overweight",
                               30.0 <= bmi & bmi < 34.999 ~ "Obese",
                               35.0 <= bmi & bmi < 39.999 ~ "Severe obesity",
                               40 <= bmi ~ "Morbid obesity"))

table_clean %>%
  ggplot(aes(x = income, fill=factor(BMI_class, levels=c("Underweight","Normal weight", "Overweight","Obese", "Severe obesity", "Morbid obesity")))) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(fill = "Categories")

#BMI Class distribution among diabetic people
table_clean %>%
  filter(dx == "1") %>%
  ggplot(aes(x = income, fill=factor(BMI_class, levels=c("Underweight","Normal weight", "Overweight","Obese", "Severe obesity", "Morbid obesity")))) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(fill = "Categories")

#normal range for the hemoglobin A1c level is between 4% and 5.6%. Hemoglobin A1c levels between 5.7% and 6.4% mean you have a higher chance of getting diabetes. Levels of 6.5% or higher mean you have diabetes.
#Table displaying basic statistics (mean, std, percentiles) 

#the average age, weight, bmi and glycohemoglobin for people with diabetes
table_clean %>%
  filter(dx == '1') %>%
  summarize(average_age = mean(age), average_weight = mean(wt), 
            average_bmi = mean(bmi), average_gh = mean(gh))

#number of people with untreated diabetes 
table_clean %>%
  filter(dx == '1') %>%
  filter(tx == '0') %>%
  count()






