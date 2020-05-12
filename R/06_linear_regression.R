# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse)
library(styler)
library(yaml)

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
prostate_data <- read_tsv(file = "data/03_prostate_and_tcga_joined.tsv")

# Wranglisng data
# ------------------------------------------------------------------------------
prostate_ds_only <- prostate_data %>%
  filter(dataset == 0)


# Construct model
# ------------------------------------------------------------------------------
mdl_continuous_variables <- prostate_ds_only %>%
  lm(
    formula = months_fu ~ age +
      weight_index +
      sbp +
      dbp +
      serum_hg +
      tumour_size +
      ap,
    data = .
  )

#plot(mdl_continuous_variables)

# Print the model's statistical summary (Coefficients, min, max...)
summary(mdl_continuous_variables)


# Write data
# ------------------------------------------------------------------------------
