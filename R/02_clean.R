# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library("tidyverse")
library("chron")
library("date")

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
prostate <- read_tsv(file = "data/01_prostate.tsv")

# Wrangle data
# ------------------------------------------------------------------------------
# changing column names
col_names <- c("Patient_ID", "Stage", "Estrogen(mg)", "Months_FU", "Status", "Age", "Weight_index", "Activity_level", "CV_history", "SBP", "DBP", "EKG", "Serum_HG", "Tumour_size", "SG", "AP", "Bone_metastases", "Date_on_study")
colnames(prostate) <- col_names

prostate <- as_tibble(prostate)

prostate_data <- prostate %>%
   na.exclude(.)         #removing the rows which contain at least one NA

# #split the estrogen (mg) col into two cols + deletion of the Estr col, 
# #since it contains only the word "estrogen"
prostate_data <- prostate_data %>%
  mutate(`Estrogen(mg)` = str_replace_all(string = `Estrogen(mg)`, pattern = "placebo", replacement = "0.0")) %>%  
  separate(data = ., col = `Estrogen(mg)`, into = c("Estrogen(mg)", "Estr"), sep = "mg") %>%
  dplyr::select(-Estr)

prostate_data <- prostate_data %>%
  mutate(`Estrogen(mg)`= as.numeric(x = `Estrogen(mg)`, digits = 2))

prostate_data <- prostate_data %>% 
  mutate("Date_on_study" = as.Date(x = `Date_on_study`, origin = "1960-01-01"))

# Write data
# ------------------------------------------------------------------------------
write_tsv(x = prostate_data,
          path = "data/02_prostate_data_clean.tsv")