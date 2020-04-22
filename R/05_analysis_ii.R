# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library("tidyverse")
library("ggcorrplot")

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
prostate_one_hot <- read_tsv(file = "data/03_prostate_one_hot_status3.tsv")

# Wrangle data
# ------------------------------------------------------------------------------
prostate_one_hot_corr <- prostate_one_hot %>% 
  select(-Date_on_study, - Patient_ID) %>% 
  cor(.)

# Model data
# ------------------------------------------------------------------------------


# Visualise data
# ------------------------------------------------------------------------------
corr_matrix <- ggcorrplot(prostate_one_hot_corr, hc.order = TRUE, type = "lower", title = "Correlation matrix", tl.cex = 4, 
                          lab = TRUE, lab_size = 1)

# Write data
# ------------------------------------------------------------------------------
# write_tsv(...)
ggsave("results/05_corr_matrix.png", corr_matrix)