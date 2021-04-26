library("tidyverse")


table <- read.table(file = '/cloud/project/data/nhgh.tsv', sep = '\t', header = TRUE) 


table_clean <- table %>%
  drop_na()



ggplot(data = table_clean,
       mapping = aes(x = income,
                     y = mean(bmi),
                     fill = sex,
                     color = sex)) +
  geom_point()
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
