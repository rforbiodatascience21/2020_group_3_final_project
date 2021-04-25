# Converting Rdata-file to CSV

# Loading data
library(tidyverse)
load("/cloud/project/data/chiaretti.RData")

#Extracting variables
x <- chiaretti %>%
  pluck("x") %>%
  as_tibble()
y <- chiaretti %>%
  pluck("y") %>%
  as_tibble()

# Merging into one variable
complete_data <- bind_cols(x,y)

# Writing csv
write.csv(complete_data,"/cloud/project/data/chiaretti.csv", row.names = FALSE)

View(complete_data)