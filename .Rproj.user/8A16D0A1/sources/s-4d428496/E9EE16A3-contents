library(tidyverse)
load("/cloud/project/data/chiaretti.RData")
x = as_tibble(chiaretti[["x"]])
y = as_tibble(chiaretti[["y"]])
leukemia_data = as_tibble(x,y)

# Converting Rdata-file to CSV

# Loading data
load("chiaretti.RData")

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
write.csv(complete_data,"chiaretti.csv", row.names = FALSE)
