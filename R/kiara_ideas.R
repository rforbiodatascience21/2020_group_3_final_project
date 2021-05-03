library("tidyverse")
library("dplyr")
library("tidyr")
my_data <- read_csv2(file = "/cloud/project/data/untidy_t1_diabetes.csv")
my_data_clean <- my_data %>%
  mutate(Age = case_when(Age == "greater then 15" ~ "> 15",
                         Age == "Less then 11" ~ "< 11",
                         Age == "Less then 15" ~"< 15",
                         Age == "Less then 5" ~ "< 5"),
         HbA1c = case_when(HbA1c == "Over 7.5%" ~"> 7.5%",
                           HbA1c == "Less then 7.5%" ~"< 7.5%"),
         BMI = round(BMI,1))

# Wrangle data ------------------------------------------------------------
my_data_clean_aug <- my_data_clean %>%
  mutate(Dur_disease = str_extract(`Duration of disease`,"\\d+\\.?\\d*"),
         unit = str_replace(`Duration of disease`, Dur_disease,"")) %>%
  select(-`Duration of disease`)

# Converting duration to days for every value
my_data_clean_aug <- my_data_clean %>%
  mutate(Dur_disease = as.numeric(Dur_disease)) %>%
  mutate(Dur_disease = case_when(unit == "d" ~ Dur_disease,
                                 unit == "w" ~ Dur_disease * 7,
                                 unit == "m" ~ Dur_disease * 30,
                                 unit == "y" ~ Dur_disease * 365)) %>%
  separate(`Other diease`,
           into = c("first_disease",
                    "second_disease",
                    "third_disease"),
           sep = ",") %>%
  mutate(first_disease = case_when(first_disease == "no" ~ "none",
                                   first_disease != "no" ~ first_disease),
         second_disease = replace_na(second_disease, "none"),
         third_disease = replace_na(third_disease, "none")) %>%
  select(-unit)

# Splitting data
diseases <- my_data_clean_aug %>%
  select(first_disease,
         second_disease,
         third_disease)

data <- my_data_clean_aug %>%
  select(-first_disease,
         -second_disease,
         -third_disease)

Tibble1 <- my_data_clean_aug %>%
  select(BMI,`Insulin taken`)
Tibble2 <- my_data_clean_aug %>%
  select(BMI, `Impaired glucose metabolism`)

DemonstratingJoin <- full_join(x = Tibble1,
                               y = Tibble2,
                               by = "BMI")

#average BMI of people with diabetes
my_data_clean_aug %>%
  filter(Affected == 'yes') %>%
  summarise(mean(BMI))
  
#number of people with untreated diabetes 
my_data_clean_aug %>%
  filter(Affected == 'yes') %>%
  filter(`Insulin taken` == 'No') %>%
  count()
#everybody is treated

my_data_clean_aug <- my_data_clean_aug %>%
  mutate(Diabetes = case_when(Affected == 'yes' ~ '1',
                              Affected == 'No' ~ '0'))

my_data_clean_aug %>% 
  ggplot(aes(x=Affected, y=BMI, fill=Affected)) + 
  geom_boxplot(width=0.5,lwd=0.5)+
  labs(title="BMI of people with and without diabetes",
       x="Affected with diabetes?",
       y="BMI")

my_data_clean_aug <- my_data_clean_aug %>%
  mutate(BMI_class = case_when(BMI < 18.5 ~ "underweight",
                               18.5 <= BMI & BMI < 25  ~ "normal weight",
                               25 <= BMI & BMI < 30  ~ "overweight",
                               30 <= BMI & BMI < 35 ~ "obese",
                               35 <= BMI & BMI < 40 ~ "severe obesity",
                               40 <= BMI ~ "morbid obesity"))
#BMI Class distribution among diabetic people
my_data_clean_aug %>%
  filter(Diabetes == "1") %>%
  ggplot(aes(x = Sex, fill=factor(BMI_class, levels=c("Underweight","Normal weight", "Overweight","Obese", "Severe obesity", "Morbid obesity")))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(fill = "Categories")

#normal range for the hemoglobin A1c level is between 4% and 5.6%. Hemoglobin A1c levels between 5.7% and 6.4% mean you have a higher chance of getting diabetes. Levels of 6.5% or higher mean you have diabetes.
#Table displaying basic statistics (mean, std, percentiles) 

#the average age, weight, BMI and glycohemoglobin for people with diabetes
#table_clean %>%
 # filter(dx == '1') %>%
 # summarize(average_age = mean(age), average_weight = mean(wt), 
     #       average_BMI = mean(BMI), average_gh = mean(gh))








