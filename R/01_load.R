# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse)
library(foreign)

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
prostate_raw <- read.dta("data/_raw/prostate.dta")
TCGA_prostate_raw <- read.table("data/_raw/TCGA-PRAD.GDC_phenotype.tsv", sep = "\t", header = T, quote = "", na.strings=c("","NA"))
TCGA_prostate_survival_raw <- read.table("data/_raw/TCGA-PRAD.survival.tsv", sep = "\t", header = T, quote = "", na.strings=c("","NA"))

#Wrangle data
#-------------------------------------------------------------------------------

prostate_raw <- prostate_raw %>% 
  as_tibble()
TCGA_prostate_raw <- TCGA_prostate_raw %>% 
  as_tibble()
TCGA_prostate_survival_raw <- TCGA_prostate_survival_raw %>% 
  as_tibble()

# Write data
# ------------------------------------------------------------------------------
write_tsv(x = prostate_raw,
          path = "data/01_prostate.tsv")
write_tsv(x = TCGA_prostate_raw,
          path = "data/01_TCGA_prostate.tsv")
write_tsv(x = TCGA_prostate_survival_raw,
          path = "data/01_TCGA_prostate_survival.tsv")
