# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse)
library(foreign)
library(styler)
# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
prostate_raw <- read.dta("data/_raw/prostate.dta")
tcga_prostate_raw <- read_tsv("data/_raw/TCGA-PRAD.GDC_phenotype.tsv")
tcga_prostate_survival_raw <- read_tsv("data/_raw/TCGA-PRAD.survival.tsv")
# TCGA data already in tibble format

# Write data
# ------------------------------------------------------------------------------
write_tsv(
  x = prostate_raw,
  path = "data/01_prostate.tsv"
)
write_tsv(
  x = tcga_prostate_raw,
  path = "data/01_TCGA_prostate.tsv"
)
write_tsv(
  x = tcga_prostate_survival_raw,
  path = "data/01_TCGA_prostate_survival.tsv"
)

