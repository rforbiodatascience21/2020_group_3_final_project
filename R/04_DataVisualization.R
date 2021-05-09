# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library("ggrepel")
library("patchwork")

# Load data ---------------------------------------------------------------
my_data_clean_aug <- read_tsv(file = "data/03_my_data_clean_aug.tsv")

# Data Visualization ------------------------------------------------------

# Scatter Plot 1
Scatter1 <- my_data_clean_aug %>% 
  filter(Dur_disease != "0") %>%
  ggplot(mapping = aes(x= Dur_disease,
                       y= BMI,
                       color = Sex)) +
  geom_point() +
  scale_color_discrete(name = "Gender") +
  theme_cowplot(12) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title="Duration of Disease w.r.t variables Height and Gender", x="Disease Duration (Days)", y="BMI")
Scatter1

# Scatter Plot 2
Scatter2 <- my_data_clean_aug %>% 
  filter(second_disease_categories != "none") %>%
  ggplot(mapping = aes(x= Age,
                       y= Weight,
                       color = second_disease_categories)) +
  geom_text_repel(aes(label = second_disease_categories), size =3) +
  geom_point() +
  scale_color_discrete(name = "Diseases") +
  theme_cowplot(12) +
  theme(plot.title = element_text(hjust = 0.5)) +
        labs(title="2 accompanying diseases", x="Age (Range: Years)", y="Weight (Kgs)")
Scatter2

# Scatter Plot 3
Scatter3 <- my_data_clean_aug %>% 
  filter(third_disease_categories != "none") %>%
  ggplot(mapping = aes(x= Age,
                       y= Weight,
                       color = third_disease_categories)) +
  geom_text_repel(aes(label = third_disease_categories), size =3) +
  geom_point() +
  theme_cowplot(12) +
  scale_color_discrete(name = "Diseases") +
  theme(plot.title = element_text(hjust = 0.5)) +
        labs(title="3 accompanying diseases", x="Age (Range: Years)", y="Weight (Kgs)")
Scatter3

# Patchwork (Stacking the last two scatter plots into one plot)
P <- Scatter2 / Scatter3
P

hist <- my_data_clean_aug %>%
  filter(Dur_disease > 0) %>% # filtering out individuals that do not have T1 diabetes
  ggplot(mapping = aes(x = Dur_disease,
                       fill = `Area of Residence`,
                       color = `Area of Residence`)) +
  geom_histogram(alpha = 0.5,
                 bins = 25) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5)) + # repositioning legend to be at the bottom
  labs(title = "Distribution of duration of based on residency",
       x = "Duration of T1 diabetes (days)",
       y = "Number of individuals")
hist

# Density plot - Distribution of weights across BMI classes stratified on
# stratified on Age and whether the individuals have T1 diabtes or not
dens <- ggplot(data = my_data_clean_aug,
               mapping = aes(x = Weight,
                             y = BMI_class,
                             fill = Age)) +
  facet_wrap("Affected") +
  scale_fill_viridis_d(labels = c("< 5",
                                  "< 11",
                                  "< 15",
                                  "> 15")) + # reordering the legend labels
  scale_y_discrete(limits = rev(c("underweight",
                                  "normal weight",
                                  "overweight",
                                  "obese",
                                  "severe obesity",
                                  "morbid obesity"))) + # reordering the y-labels
  geom_density_ridges(alpha=0.5) +
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5)) + # placing legend at the bottom and centering plot title
  labs(title = "Weight distribution across BMI classes stratified on T1 diabetes",
       x = "Weight",
       y = "BMI class") # adding custom labels and title
dens

# write data ---------------------------------------------------------------
ggsave(plot = Scatter1, filename = "results/Scatter1.png")
ggsave(plot = Scatter2, filename = "results/Scatter2.png")
ggsave(plot = Scatter3, filename = "results/Scatter3.png")
ggsave(plot = P, filename = "results/ScatterPatch.png")
ggsave(plot = hist, filename = "results/Histogram.png")
ggsave(plot = dens, filename = "results/densities.png")