# Run all scripts ---------------------------------------------------------
source(file = "R/01_load.R")
source(file = "R/02_clean.R")
source(file = "R/03_augment.R")
source(file = "R/04_DataVisualization.R")
source(file = "R/05_PCA.R")
source(file = "R/06_lr_model.R")
rmarkdown::render(input = "doc/Final_Project.Rmd",
                  output_dir = "doc")
