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

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
prostate_raw <- read_tsv(file = "data/01_prostate.tsv")
TCGA_prostate_raw <- read_tsv(file = "data/01_TCGA_prostate.tsv")
TCGA_prostate_survival_raw <- read_tsv(file = "data/01_TCGA_prostate_survival.tsv")

# Wrangle data
# ------------------------------------------------------------------------------

## Prostate_raw modifications
# changing column names
prostate <- prostate_raw %>% 
  mutate("Dataset" = "prostate") #column which identifies the dataset (it will be useful later)

col_names <- c("Patient_ID", "Stage", "Estrogen_mg", 
               "Months_FU", "Status", "Age", "Weight_index",
               "Activity_level", "CV_history", "SBP", "DBP",
               "EKG", "Serum_HG", "Tumour_size", "SG", "AP",
               "Bone_metastases", "Date_on_study","Dataset")

colnames(prostate) <- col_names

#removing the rows which contain at least one NA
prostate_data <- prostate %>%
  drop_na() %>%
  filter(AP < 100)

# split the estrogen (mg) col into two cols + deletion of the Estr col, 
# since it contains only the word "estrogen"
prostate_data <- prostate_data %>%
  mutate(Estrogen_mg = str_replace(string = Estrogen_mg, pattern = "placebo", replacement = "0.0")) %>%  
  separate(data = ., col = Estrogen_mg, into = c("Estrogen_mg", "Estr"), sep = "mg") %>%
  select(-Estr)

prostate_data <- prostate_data %>%
  mutate(Estrogen_mg = as.numeric(x = Estrogen_mg, digits = 2))


#Transfom the data variable in a more readable version
prostate_data <- prostate_data %>% 
  mutate(Date_on_study = as_date(x = Date_on_study, origin = "1960-01-01"))

# Now we modify the Status column assigning a number to each possible status
# 0 - alive
# 1 - dead from prostatic cancer 
# 2 - dead from heart or CV disease
# 3 - dead from cerebrovascular accident
# 4 - dead from pulmonary embolus
# 5 - dead from other cancer 
# 6 - dead from respiratory disease
# 7 - dead from other specific non-cancer case
# 8 - dead from unspecific non-cancer cause
# 9 - dead from unknown cause

prostate_data <- prostate_data %>% 
  mutate("Status" = case_when(Status == "alive" ~ 0,
                              Status == "dead - other ca" ~ 1,
                              Status == "dead - cerebrovascular" ~ 2,
                              Status == "dead - prostatic ca" ~ 3,
                              Status == "dead - heart or vascular" ~ 4,
                              Status == "dead - pulmonary embolus" ~ 5,
                              Status == "dead - other specific non-ca" ~ 6,
                              Status == "dead - unknown cause" ~ 7,
                              Status == "dead - unspecified non-ca" ~ 8,
                              Status == "dead - respiratory disease" ~ 9))

#prostate_data <- prostate_data %>% 
#  mutate("Status" = as.integer(prostate_data$Status) -1) 
#example of faster but non tidy code

## Working in the TCGA datasets

# Modify 1st colum to Sample_ID (we will use it as key column)
colnames(TCGA_prostate_raw)[1] <- "Sample_ID"
colnames(TCGA_prostate_survival_raw)[1] <- colnames(TCGA_prostate_raw)[1] 

# Check the Sample_ID are the same in both tibbles
!any(TCGA_prostate_raw$Sample_ID %in% TCGA_prostate_survival_raw$Sample_ID)

# Left-join: join the phenotipe with the survival rates usng the Sample ID as key column
# Add Dataset column to identify the origin of the data
TCGA_prostate_tot <- left_join(x = TCGA_prostate_raw, y = TCGA_prostate_survival_raw, by = "Sample_ID") %>% 
  mutate("Dataset" = "TCGA") 

# Common columns in both datasets:

# prostate data          TCGA
# Patient id            patient_id
# Age                   age_at_initial_pathologic_diagnosis
# bone_metastasis       bone_scan_results --> retain only normal + abnormal == 0, prostate cancer == 1, while equivocal == NA. 
# monts of FU           OS.time --> those are days, we have to transform them in months
# SG correlates with gleason score, primary_pattern and secondary_pattern                   
  
# Select the interesting columns, change names if necessary (Patient_ID),
# and add data that can be extracted from the existing columns (SG)
TCGA_prostate_def <- TCGA_prostate_tot %>% 
  mutate('SG'=case_when(gleason_score<=6 ~ gleason_score+1,
                        gleason_score==7 & primary_pattern == 3 ~  gleason_score+2,
                        gleason_score==7 & primary_pattern == 4 ~  gleason_score+3,
                        gleason_score == 8 ~ gleason_score+4,
                        gleason_score>=9 ~ gleason_score+5)) %>% 
  select("Patient_ID" = patient_id, age_at_initial_pathologic_diagnosis, bone_scan_results, OS.time, SG, Dataset) 
  
# Remove duplicated rows
TCGA_prostate_def <- TCGA_prostate_def %>%
  unique()

# Transform data format to allow join
prostate_data <- prostate_data %>%
  mutate(Patient_ID = as.character(Patient_ID))

#Convert the content of the OS.time column from days to months. 
TCGA_prostate_def <- TCGA_prostate_def %>% 
  mutate("Months_FU" = round(OS.time/30, digits = 0), OS.time = NULL) %>% 
  select(Patient_ID, age_at_initial_pathologic_diagnosis, Months_FU, bone_scan_results, SG, Dataset) 

TCGA_prostate_def <- TCGA_prostate_def %>% 
  mutate("Bone_metastases" = (case_when(
    bone_scan_results == "Normal (no evidence of prostate cancer) [cM0]" ~ 0, 
    bone_scan_results == "Abnormal (not related to prostate cancer)" ~ 0, 
    bone_scan_results == "Prostate Cancer Metastases Present [cM1b]" ~ 1)), 
    bone_scan_results = NULL) 

TCGA_prostate_final <- TCGA_prostate_def %>%
  select(Patient_ID, age_at_initial_pathologic_diagnosis, Months_FU, Bone_metastases, SG, Dataset)


colnames(TCGA_prostate_final) <- c("Patient_ID", "Age", "Months_FU", "Bone_metastases", "SG", "Dataset")

prostate_tidy <- full_join(x = prostate_data, y = TCGA_prostate_final, by = c("Patient_ID", "Age", "Months_FU", "Bone_metastases", "SG", "Dataset")) %>% 
  select(Patient_ID, Age, Months_FU, Bone_metastases, everything())

# Write data
# ------------------------------------------------------------------------------
write_tsv(x = prostate_data,
          path = "data/02_prostate_data_clean.tsv")

