# Load data
my_data_clean_aug <- read_tsv(file = "data/03_my_data_clean_aug.tsv")

# Scatter Plot
# Creating binary values for accompanying diseases
my_data_clean_aug <- my_data_clean_aug %>%
  mutate(other_disease_binary = case_when(first_disease == "none" ~ 0,
                                          first_disease != "none" ~ 1,
                                          second_disease == "none" ~ 0,
                                          second_disease != "none" ~ 1,
                                          third_disease == "none" ~ 0,
                                          third_disease != "none" ~ 1,))
my_data_clean_aug

Scatter1 <- my_data_clean_aug %>% 
  filter(Dur_disease != "0")%>%
  ggplot(mapping = aes(x= Dur_disease,
                       y= Height,
                       fill = Sex,
                       color = Sex))+
  
  geom_point() +
  geom_boxplot(alpha = 0) +
  geom_jitter(alpha = 0.5) +
  theme_cowplot(12)+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Duration of Disease w.r.t variables Height and Sex", x="Disease Duration (Days)", y="Height (m)")
Scatter1

#write_tsv(...)
ggsave(plot = Scatter1, filename = "results/Scatter1.png")