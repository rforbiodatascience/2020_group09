# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse) 
library(styler)

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
  filter(ap < 100)

# creating categorical column cat_status describing the causes of death
prostate_data <- prostate_data %>%
  mutate("status" = (case_when(
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


# in the following lines we transform the dataset into a more useful format 
# to take in to account of the categorical variables in the analysis

prostate_one_hot <- prostate_data %>% 
  mutate(ekg = str_replace_all(string = ekg, pattern = "&", 
                               replacement = "")) %>%
  mutate(ekg = str_replace_all(string = ekg, pattern = " ",
                               replacement = "_")) %>%
  mutate(ekg = str_to_lower(string = ekg, locale = "en"))

prostate_one_hot <- one_hot_encoder(prefix = "ekg_", 
                                    dataset = prostate_one_hot, 
                                    colname = "ekg")

prostate_one_hot <- prostate_one_hot %>% 
  mutate(activity_level = str_replace_all(string = activity_level , 
                                          pattern = ">",
                                          replacement = "more_than"))%>%
  mutate(activity_level = str_replace_all(string = activity_level , 
                                          pattern = "<",
                                          replacement = "less_than"))%>%
  mutate(activity_level = str_replace_all(string = activity_level , 
                                          pattern = "% daytime", 
                                          replacement = "percent"))%>%
  mutate(activity_level = str_replace_all(string = activity_level , 
                                          pattern = " ", 
                                          replacement = "_"))

prostate_one_hot <- one_hot_encoder(prefix = "activity_", 
                                    dataset = prostate_one_hot, 
                                    colname = "activity_level")

# one hot with three status classes: alive, dead - prostate ca, dead_other 

prostate_one_hot <- prostate_one_hot %>% 
  mutate(cat_status = status) %>%
  mutate(cat_status_nominal = case_when(
    status == 0 ~ "Alive",
    status == 2 ~ "Death other",
    status == 1 ~ "Death prostate cancer"
  ))


prostate_one_hot <- one_hot_encoder(dataset = prostate_one_hot, 
                                    prefix = "status_", 
                                    colname = "status")

prostate_one_hot <- prostate_one_hot %>% 
  rename("status_dead_other" = "status_2",
         "status_dead_prostatic_ca" = "status_1",
         "status_alive" = "status_0" )

prostate_one_hot <- prostate_one_hot %>% 
  mutate("patient_id" = as.character(patient_id))

## Augment tcga datasets

# We need to join the two TCGA datasets. To be sure that all the IDs in the sample_id column of the two TCGA datasets 
# are the same, and that we are not losing data, we use the anti_join funtion, with sample_id as the key column. 
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
  mutate("dataset" = 1, 
         sample_id = NULL) 

# Remove duplicated rows ()
tcga_unique <- tcga %>%
  unique()

# Convert the content of the os.time column from days to months. 
tcga_final <- tcga_unique %>% 
  mutate("months_fu" = round(os_time/30, digits = 0),
         os_time = NULL) 

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
                          gleason_score >= 9 ~ gleason_score + 5),
         gleason_score = NULL, 
         primary_pattern = NULL)

# One hot encoding status: alive, dead - prostata ca, dead_other
# The patient death reason can be Prostate Cancer or NA. 
# If its dead but not by Prostate Cancer we can assume is for
# another reason
tcga_final <- tcga_final %>%
  mutate(status = case_when(vital_status_demographic == "Alive" ~ "alive",
                            vital_status_demographic == "Dead" & patient_death_reason == "Prostate Cancer" ~ "dead_prostatic_ca",
                            vital_status_demographic == "Dead" & is.na(patient_death_reason) ~ "dead_other"
  ), 
  vital_status_demographic = NULL, 
  patient_death_reason = NULL)

tcga_final <- tcga_final %>%
  mutate(cat_status = (case_when(
    status == "alive" ~ 0,
    status == "dead_prostatic_ca" ~ 1, 
    status == "dead_other" ~ 2)))%>%
  mutate(cat_status_nominal = case_when(
    status == "alive" ~ "Alive",
    status == "dead_prostatic_ca" ~ "Death prostate cancer",
    status == "dead_other" ~ "Death other"
  ))

tcga_final <- tcga_final %>% 
  drop_na(cat_status)

tcga_final <- one_hot_encoder(dataset = tcga_final, 
                              prefix = "status_", 
                              colname = "status")


# Select the interesting columns, 
# and add data that can be extracted from the existing columns (sg)
prostate_final <- full_join(x = prostate_one_hot,
                            y = tcga_final, 
                            by = c("patient_id", "months_fu", 
                                   "age", "sg", "bone_metastases", 
                                   "dataset", "cat_status", "cat_status_nominal" , 
                                   "status_alive", "status_dead_prostatic_ca", 
                                   "status_dead_other"))

prostate_final <- prostate_final %>% 
  select(patient_id, months_fu, age, sg, bone_metastases, cat_status, 
         cat_status_nominal, status_alive, status_dead_prostatic_ca, 
         status_dead_other, dataset, everything())

# ------------------------------------------------------------------------------

write_tsv(x = prostate_final,
          path = "data/03_prostate_and_tcga_joined.tsv")

