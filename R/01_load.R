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
tcga_prostate_raw <- read.table("data/_raw/TCGA-PRAD.GDC_phenotype.tsv", sep = "\t", header = T, quote = "", na.strings=c("","NA"))
tcga_prostate_survival_raw <- read.table("data/_raw/TCGA-PRAD.survival.tsv", sep = "\t", header = T, quote = "", na.strings=c("","NA"))
#TCGA data already in tibble format 

# Write data
# ------------------------------------------------------------------------------
write_tsv(x = prostate_raw,
          path = "data/01_prostate.tsv")
write_tsv(x = tcga_prostate_raw,
          path = "data/01_TCGA_prostate.tsv")
write_tsv(x = tcga_prostate_survival_raw,
          path = "data/01_TCGA_prostate_survival.tsv")
