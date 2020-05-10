# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(stringr)
library(chron)
library(lubridate)
library(haven)
library(styler)

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
prostate_raw <- read_tsv(file = "data/01_prostate.tsv")
tcga_prostate_raw <- read_tsv(file = "data/01_TCGA_prostate.tsv")
tcga_prostate_survival_raw <- read_tsv(file = "data/01_TCGA_prostate_survival.tsv")

# Wrangle data
# ------------------------------------------------------------------------------

## Prostate_raw cleaning

# changing column names
prostate_clean <- prostate_raw %>%
  rename(
    "patient_id" = "patno", "estrogen_mg" = "rx", "months_fu" = "dtime",
    "weight_index" = "wt", "activity_level" = "pf", "cv_history" = "hx",
    "serum_hg" = "hg", "tumour_size" = "sz", "bone_metastases" = "bm",
    "date_on_study" = "sdate"
  )

# removing the rows which contain at least one NA

# removing NA 999 AP
prostate_clean <- prostate_clean %>%
  drop_na() %>%
  filter(ap != 999)

# delete string " mg estrogen" from column estrogen_mg
prostate_clean <- prostate_clean %>%
  mutate(estrogen_mg = str_replace(
    string = estrogen_mg, pattern = "placebo",
    replacement = "0.0"
  )) %>%
  mutate(estrogen_mg = str_remove_all(string = estrogen_mg, pattern = " mg estrogen"))

# Change format
# prostate_clean <- prostate_clean %>%
#  mutate(estrogen_mg = as.numeric(x = estrogen_mg, digits = 2))

# Transfom the data variable in a more readable version
prostate_clean <- prostate_clean %>%
  mutate(date_on_study = as_date(x = date_on_study, origin = "1960-01-01"))


## Cleaning TCGA datasets
tcga_prostate_clean <- tcga_prostate_raw %>%
  rename(
    "sample_id" = "submitter_id.samples",
    "age" = "age_at_initial_pathologic_diagnosis",
    "vital_status_demographic" = "vital_status.demographic"
  )

tcga_prostate_survival_clean <- tcga_prostate_survival_raw %>%
  rename(
    "sample_id" = "sample", "os" = "OS",
    "patient_id" = "_PATIENT", "os_time" = "OS.time"
  )

# selecting variables of interest and removing NAs from all the columns except for
# the patient_death_reason, because altough this column is almost empty, we still need 
# it in the augment. 

tcga_prostate_clean <- tcga_prostate_clean %>%
  select(sample_id, patient_id, bone_scan_results, age,
    gleason_score, primary_pattern, vital_status_demographic,
    patient_death_reason
  ) %>% 
  drop_na(sample_id, patient_id, bone_scan_results, age,
          gleason_score, primary_pattern, vital_status_demographic)


tcga_prostate_survival_clean <- tcga_prostate_survival_clean %>%
  select(-c("os", "patient_id")) %>%
  drop_na()

# Write data
# ------------------------------------------------------------------------------
write_tsv(
  x = prostate_clean,
  path = "data/02_prostate_clean.tsv"
)
write_tsv(
  x = tcga_prostate_clean,
  path = "data/02_tcga_prostate_clean.tsv"
)
write_tsv(
  x = tcga_prostate_survival_clean,
  path = "data/02_tcga_survival_prostate_clean.tsv"
)
