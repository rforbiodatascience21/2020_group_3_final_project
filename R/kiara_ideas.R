library("tidyverse")


table <- read.table(file = '/cloud/project/data/nhgh.tsv', sep = '\t', header = TRUE) 

View(table)
table_clean <- table %>%
  drop_na()

View(table_clean)