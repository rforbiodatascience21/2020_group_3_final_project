library("ggrepel")
library("patchwork")

# Load data
my_data_clean_aug <- read_tsv(file = "data/03_my_data_clean_aug.tsv")


# Creating binary values for accompanying diseases

my_data_clean_aug <- my_data_clean_aug %>%
  mutate(other_disease_binary = case_when(first_disease == "none" ~ 0,
                                          first_disease != "none" ~ 1,
                                          second_disease == "none" ~ 0,
                                          second_disease != "none" ~ 1,
                                          third_disease == "none" ~ 0,
                                          third_disease != "none" ~ 1,))

# Using the right spelling and categories for second disease

my_data_clean_aug <- my_data_clean_aug %>%
  mutate(second_disease_categories = case_when(second_disease == "Allergic problem" ~ "Allergies",
                                              second_disease == "allergic problem" ~ "Allergies",
                                              second_disease == "body" ~ "Body Pain",
                                              second_disease == "pain problem" ~ "Body Pain",
                                              second_disease == "eye problem" ~ "Optical Issues",
                                              second_disease == "eye" ~ "Optical Issues",
                                              second_disease == "screen problem" ~ "Optical Issues",
                                              second_disease == "ear problem" ~ "Ear Issues",
                                              second_disease == "head problem" ~ "Headache",
                                              second_disease == "head pain" ~ "Headache",
                                              second_disease == "none" ~ "none",
                                              second_disease == NA ~ "none"))

# Using the right spelling and categories for third disease

my_data_clean_aug <- my_data_clean_aug %>%
  mutate(third_disease_categories = case_when(third_disease == "hormon" ~ "Hormone Irregularities",
                                              third_disease == "none" ~ "none"))

# Scatter Plot 1

Scatter1 <- my_data_clean_aug %>% 
  filter(Dur_disease != "0")%>%
  ggplot(mapping = aes(x= Dur_disease,
                       y= BMI,
                       color = Sex))+
  
  geom_point() +
  scale_color_discrete(name = "Gender") +
  #geom_boxplot(alpha = 0) +
  #geom_jitter(alpha = 0.5) +
  theme_cowplot(12)+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Duration of Disease w.r.t variables Height and Gender", x="Disease Duration (Days)", y="BMI")
Scatter1

# Scatter Plot 2

Scatter2 <- my_data_clean_aug %>% 
  filter(second_disease_categories != "none") %>%
  ggplot(mapping = aes(x= Age,
                       y= Weight,
                       color = second_disease_categories))+
  geom_text_repel(aes(label = second_disease_categories), size =3)+
  geom_point() +
  scale_color_discrete(name = "Diseases") +
  theme_cowplot(12)+
  theme(plot.title = element_text(hjust = 0.5))+
        labs(title="2 accompanying diseases", x="Age (Range: Years)", y="Weight (Kgs)")
Scatter2


# Scatter Plot 3

Scatter3 <- my_data_clean_aug %>% 
  filter(third_disease_categories != "none") %>%
  ggplot(mapping = aes(x= Age,
                       y= Weight,
                       color = third_disease_categories))+
  geom_text_repel(aes(label = third_disease_categories), size =3)+
  geom_point() +
  theme_cowplot(12)+
  scale_color_discrete(name = "Diseases") +

  theme(plot.title = element_text(hjust = 0.5))+
        labs(title="3 accompanying diseases", x="Age (Range: Years)", y="Weight (Kgs)")
Scatter3

# Patchwork

P <- Scatter2 / Scatter3
P

#Save_Images(...)
ggsave(plot = Scatter1, filename = "results/Scatter1.png")
ggsave(plot = Scatter2, filename = "results/Scatter2.png")
ggsave(plot = Scatter3, filename = "results/Scatter3.png")
ggsave(plot = P, filename = "results/ScatterPatch.png")