# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse)

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
  mutate("dataset" = 0)

prostate_data <- prostate_data %>%
  mutate("cat_status" = (case_when(
    status == "alive" ~ 0,
    status == "dead - prostatic ca" ~ 1, 
    status == "dead - cerebrovascular" ~ 2, 
    status == "dead - heart or vascular" ~ 2,
    status == "dead - pulmonary embolus" ~ 2,
    status == "dead - other ca" ~ 2,
    status == "dead - other specific non-ca" ~ 2,
    status == "dead - unspecified non-ca" ~ 2,
    status == "dead - respiratory disease" ~ 2,
    status == "dead - unknown cause" ~ 2)))


# in the following lines we transform the dataset into a more useful format to perform the machine learning analysis. 
# in particular .....

#prostate_one_hot <- one_hot_encoder("ekg", prefix = "ekg_", dataset = prostate_data)

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
  mutate(`status_dead_other` = `status_dead - cerebrovascular` | 
           `status_dead - heart or vascular` | 
           `status_dead - pulmonary embolus` | 
           `status_dead - other ca` |
           `status_dead - other specific non-ca` | 
           `status_dead - unspecified non-ca` | 
           `status_dead - respiratory disease` | 
           `status_dead - unknown cause`) %>% 
  mutate(`status_dead_other`= as.numeric(`status_dead_other`)) %>% 
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
  rename("status_dead_prostatic_ca"  = "status_dead - prostatic ca")

prostate_one_hot_status3 <- prostate_one_hot_status3 %>% 
  mutate("patient_id" = as.character(patient_id))

## Augment tcga datasets

# We need to join the two TCGA datasets. To be sure that all the IDs in the sample_id column of the two TCGA datasets are the same, 
# and that we are not loosing data, we use the anti_join funtion, with sample_id as the key column. 
# We expect a tibble with 0 rows as output if all the IDs match. 
dim(anti_join(x = tcga_prostate_clean, 
              y = tcga_prostate_survival_clean,
              by = "sample_id"))

# Inner-join: join the phenotype with the survival rates using the Sample ID
# as key column. 
# Add Dataset column to identify the origin of the data
tcga <- inner_join(x = tcga_prostate_clean,
                   y = tcga_prostate_survival_clean, 
                   by = "sample_id") %>% 
  mutate("dataset" = 1)


tcga <- tcga %>%
  select(patient_id, bone_scan_results, age, os_time,
         gleason_score, primary_pattern, vital_status_demographic, 
         patient_death_reason, dataset) 

unique(tcga$bone_scan_results)

# Remove duplicated rows ()
tcga_unique <- tcga %>%
  unique()

# Convert the content of the os.time column from days to months. 
tcga_final <- tcga_unique %>% 
  mutate("months_fu" = round(os_time/30, digits = 0), os_time = NULL) 

# Binarize bone metastases results
tcga_final <- tcga_final %>% 
  mutate("bone_metastases" = (case_when(
    bone_scan_results == "Normal (no evidence of prostate cancer) [cM0]" ~ 0, 
    bone_scan_results == "Abnormal (not related to prostate cancer)" ~ 0, 
    bone_scan_results == "Prostate Cancer Metastases Present [cM1b]" ~ 1)), 
    bone_scan_results = NULL)

tcga_final <- tcga_final %>% 
  drop_na(bone_metastases)

# Obtain SG value from gleason score and primary and secondary pattern
tcga_final <- tcga_final %>% 
  mutate('sg' = case_when(gleason_score <= 6 ~ gleason_score + 1,
                          gleason_score == 7 & primary_pattern == 3 ~  gleason_score + 2,
                          gleason_score == 7 & primary_pattern == 4 ~  gleason_score + 3,
                          gleason_score == 8 ~ gleason_score + 4,
                          gleason_score >= 9 ~ gleason_score + 5))

# One hot encoding status: alive, dead - prostata ca, dead_other
# The patient death reason can be Prostate Cancer or NA. 
# If its dead but not by Prostate Cancer we can asume is for
# another reason
tcga_final <- tcga_final %>%
  mutate(status = case_when(vital_status_demographic == "Alive" ~ "alive",
                              vital_status_demographic == "Dead" & patient_death_reason == "Prostate Cancer" ~ "dead_prostatic_ca",
                              vital_status_demographic == "Dead" & is.na(patient_death_reason) ~ "dead_other"
                              ))

tcga_final <- tcga_final %>%
  mutate("cat_status" = (case_when(
    status == "alive" ~ 0,
    status == "dead_prostatic_ca" ~ 1, 
    status == "dead_other" ~ 2)))

tcga_final <- tcga_final %>% 
  drop_na(cat_status)

tcga_final <- tcga_final %>% 
  group_by(status) %>%
  mutate(count = 1) %>%
  pivot_wider(names_from = status, values_from = count, 
              names_prefix = "status_", values_fill = list(count = 0))
  

# Common columns in both datasets:

# prostate data          TCGA
# Patient id            patient_id
# Age                   age_at_initial_pathologic_diagnosis
# bone_metastasis       bone_scan_results --> retain only normal + abnormal == 0, prostate cancer == 1, while equivocal == NA. 
# monts of FU           OS.time --> those are days, we have to transform them in months
# status                status --> alive,dead by prostate cancer, dead other
# sg correlates with gleason score, primary_pattern and secondary_pattern                   

# Select the interesting columns, change names if necessary (patient_id),
# and add data that can be extracted from the existing columns (sg)
prostate_final <- full_join(x = prostate_one_hot_status3,
                           y = tcga_final, 
                           by = c("patient_id", "months_fu", "age", "sg", "bone_metastases", 
                                  "dataset", "cat_status", "status_alive", "status_dead_prostatic_ca", "status_dead_other"))

prostate_final <- prostate_final %>% 
  select(patient_id, months_fu, age, sg, bone_metastases, cat_status, status_alive, status_dead_prostatic_ca, status_dead_other, dataset, everything())

# ------------------------------------------------------------------------------

write_tsv(x = prostate_final,
          path = "data/03_prostate_and_tcga_joined.tsv")
