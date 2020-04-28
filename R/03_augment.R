# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library("tidyverse")

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
prostate_clean <- read_tsv(file = "data/02_prostate_clean.tsv")
tcga_prostate_clean <- read_tsv(file = "data/02_tcga_prostate_clean.tsv")
tcga_prostate_survival_clean <- read_tsv(file = "data/02_tcga_survival_prostate_clean.tsv")

# Wrangle data
# ------------------------------------------------------------------------------

## Augment prostate data

# Add column dataset
prostate_data <- prostate_clean %>% 
  mutate("dataset" = "prostate")

# one hot encoding factors (which are actually characters!)
prostate_one_hot <- prostate_data %>% 
  group_by(ekg) %>%
  mutate(count = 1) %>%
  pivot_wider(names_from = ekg, values_from = count, 
              names_prefix = "ekg_", values_fill = list(count = 0))

prostate_one_hot <- prostate_one_hot %>% 
  group_by(activity_level) %>%
  mutate(count = 1) %>%
  pivot_wider(names_from = activity_level, 
              values_from = count, 
              names_prefix = "activity_", 
              values_fill = list(count = 0))

prostate_one_hot_factors <- prostate_one_hot %>% 
  group_by(status) %>%
  mutate(count = 1) %>%
  pivot_wider(names_from = status, 
              values_from = count, 
              names_prefix = "status_", 
              values_fill = list(count = 0))

# one hot with three status classes: alive, dead - prostata ca, dead_other 
prostate_one_hot_status3 <- prostate_one_hot_factors %>% 
  mutate(`status_dead other` = `status_dead - cerebrovascular` | 
           `status_dead - heart or vascular` | 
           `status_dead - pulmonary embolus` | 
           `status_dead - other ca` |
           `status_dead - other specific non-ca` | 
           `status_dead - unspecified non-ca` | 
           `status_dead - respiratory disease` | 
           `status_dead - unknown cause`) %>% 
  mutate(`status_dead other`= as.numeric(`status_dead other`)) %>% 
  select(-`status_dead - cerebrovascular`, 
         - `status_dead - heart or vascular`, 
         - `status_dead - pulmonary embolus`, 
         - `status_dead - other ca`,
         - `status_dead - other specific non-ca`,  
         - `status_dead - unspecified non-ca`,
         - `status_dead - respiratory disease`, 
         - `status_dead - unknown cause`)

# Eliminate spaces in generated column names
prostate_one_hot_status3 <- prostate_one_hot_status3 %>% 
  rename("status_dead_prostatic_ca"  = "status_dead - prostatic ca",
         "status_dead_other" = "status_dead other")


## Augment tcga datasets

# Convert the content of the OS.time column from days to months., 
tcga_prostate_survival_data <- tcga_prostate_survival_clean %>% 
  mutate("months_fu" = round(os_time/30, digits = 0), os_time = NULL) 

# Binarize bone metastases results
tcga_prostate_data <- tcga_prostate_clean %>% 
  mutate("bone_metastases" = (case_when(
    bone_scan_results == "Normal (no evidence of prostate cancer) [cM0]" ~ 0, 
    bone_scan_results == "Abnormal (not related to prostate cancer)" ~ 0, 
    bone_scan_results == "Prostate Cancer Metastases Present [cM1b]" ~ 1)), 
    bone_scan_results = NULL)

# Obtain SG value from gleason score and primary and secondary pattern
tcga_prostate_data <- tcga_prostate_data %>% 
  mutate('sg' = case_when(gleason_score<=6 ~ gleason_score+1,
                          gleason_score==7 & primary_pattern == 3 ~  gleason_score+2,
                          gleason_score==7 & primary_pattern == 4 ~  gleason_score+3,
                          gleason_score == 8 ~ gleason_score+4,
                          gleason_score>=9 ~ gleason_score+5))

# One hot encoding status: alive, dead - prostata ca, dead_other
tcga_prostate_data <- tcga_prostate_data %>%
  mutate(status = case_when(vital_status_demographic == "Alive" ~ "alive",
                              vital_status_demographic == "Dead" & patient_death_reason == "Prostate Cancer" ~ "dead_prostatic_ca",
                              vital_status_demographic == "Dead" & is.na(patient_death_reason) ~ "dead_other"
                              ))

tcga_prostate_data_one_hot <- tcga_prostate_data %>% 
  group_by(status) %>%
  mutate(count = 1) %>%
  pivot_wider(names_from = status, values_from = count, 
              names_prefix = "status_", values_fill = list(count = 0))

# Select the useful data
tcga_prostate_data_subset <- tcga_prostate_data_one_hot %>%
  select(sample_id, age, bone_metastases, sg, status_alive, status_dead_prostatic_ca, status_dead_other)%>%
  drop_na()

tcga_prostate_survival_data_subset <- tcga_prostate_survival_data %>%
  select(sample_id, months_fu)
# Check the sample_id are the same in both tibbles
# anti_join(x = tcga_prostate_raw, y = tcga_prostate_raw, by = "sample_id")

# Inner-join: join the phenotipe with the survival rates using the Sample ID
# as key column. Only takes what is in both datasets
# Add Dataset column to identify the origin of the data
tcga_prostate_subset_tot <- inner_join(x = tcga_prostate_data_subset,
                                y = tcga_prostate_survival_data_subset, 
                                by = "sample_id") %>% 
  mutate("dataset" = "tcga") %>% 
  rename("patient_id" = "sample_id")

# Check !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Remove duplicated rows ()
# tcga_prostate_tot <- tcga_prostate_tot %>%
# duplicated() #unique()
# unique(tcga_prostate_tot) --> There are no duplicates

 
# Common columns in both datasets:

# prostate data          TCGA
# Patient id            patient_id
# Age                   age_at_initial_pathologic_diagnosis
# bone_metastasis       bone_scan_results --> retain only normal + abnormal == 0, prostate cancer == 1, while equivocal == NA. 
# monts of FU           OS.time --> those are days, we have to transform them in months
# status                status --> alive,dead by prostate cancer, dead other
# sg correlates with gleason score, primary_pattern and secondary_pattern                   

prostate_data_subset <- prostate_one_hot_status3 %>% 
  select(patient_id, age, bone_metastases, sg, status_alive, status_dead_prostatic_ca, status_dead_other)%>%
  mutate(patient_id = as.character(patient_id))

# Select the interesting columns, change names if necessary (patient_id),
# and add data that can be extracted from the existing columns (sg)
prostate_join <- bind_rows(x = prostate_data_subset,
                           y = tcga_prostate_subset_tot)
# ------------------------------------------------------------------------------
# write_tsv(x = prostate_data_status_in_numbers,
#           path = "data/03_prostate_status_in_numbers.tsv")

write_tsv(x = prostate_one_hot_factors,
          path = "data/03_prostate_one_hot_factors.tsv")

write_tsv(x = prostate_one_hot_status3,
          path = "data/03_prostate_one_hot_status3.tsv")

write_tsv(x = prostate_join,
          path = "data/03_prostate_and_tcga_joined")
