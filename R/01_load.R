# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library("tidyverse")
library("foreign")

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
prostate_raw <- read.dta("data/_raw/prostate.dta")


# Write data
# ------------------------------------------------------------------------------
write_tsv(x = prostate_raw,
          path = "data/01_prostate.tsv")