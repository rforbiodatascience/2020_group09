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
prostate_data <- read_tsv(file = "data/02_prostate_data_clean.tsv")

# Wrangle data
# ------------------------------------------------------------------------------

#one hot encoding factors (which are actually characters!)
prostate_one_hot_EKG <- prostate_data %>% 
  group_by(EKG) %>%
  mutate(count = 1) %>%
  pivot_wider(names_from = EKG, values_from = count, names_prefix = "EKG_", values_fill = list(count = 0))

prostate_one_hot_EKG_activity <- prostate_one_hot_EKG %>% 
  group_by(Activity_level) %>%
  mutate(count = 1) %>%
  pivot_wider(names_from = Activity_level, values_from = count, names_prefix = "Activity_", values_fill = list(count = 0))

prostate_one_hot_factors <- prostate_one_hot_EKG_activity %>% 
  group_by(Status) %>%
  mutate(count = 1) %>%
  pivot_wider(names_from = Status, values_from = count, names_prefix = "Status_", values_fill = list(count = 0))

#one hot with three status classes: alive, dead - prostata ca, dead_other 
prostate_one_hot_status3 <- prostate_one_hot_factors %>% 
  mutate(`Status_dead other` = `Status_dead - cerebrovascular` | 
           `Status_dead - heart or vascular` | 
           `Status_dead - pulmonary embolus` | 
           `Status_dead - other ca` |
           `Status_dead - other specific non-ca` | 
           `Status_dead - unspecified non-ca` | 
           `Status_dead - respiratory disease` | 
           `Status_dead - unknown cause`) %>% 
  mutate(`Status_dead other`= as.numeric(`Status_dead other`)) %>% 
  select(-`Status_dead - cerebrovascular`, 
         - `Status_dead - heart or vascular`, 
         - `Status_dead - pulmonary embolus`, 
         - `Status_dead - other ca`,
         - `Status_dead - other specific non-ca`,  
         - `Status_dead - unspecified non-ca`,
         - `Status_dead - respiratory disease`, 
         - `Status_dead - unknown cause`)
  
# ------------------------------------------------------------------------------
# write_tsv(x = prostate_data_status_in_numbers,
#           path = "data/03_prostate_status_in_numbers.tsv")

write_tsv(x = prostate_one_hot_factors,
          path = "data/03_prostate_one_hot_factors.tsv")

write_tsv(x = prostate_one_hot_status3,
          path = "data/03_prostate_one_hot_status3.tsv")
