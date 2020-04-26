# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library("tidyverse")
library("ggcorrplot")
library(broom)
# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
prostate_one_hot <- read_tsv(file = "data/03_prostate_one_hot_status3.tsv")
prostate_for_pca <- read_tsv(file = "data/03_prostate_for_pca.tsv")
view(prostate_for_pca)
# Wrangle data
# ------------------------------------------------------------------------------
prostate_one_hot_corr <- prostate_one_hot %>% 
  select(-Date_on_study, - Patient_ID) %>% 
  cor(.)

# Model data
# ------------------------------------------------------------------------------
prostate_pca <- prostate_for_pca%>%
  select(-Date_on_study, - Patient_ID, -Dataset, -Status) %>%
  prcomp(center = TRUE, scale = TRUE)
prostate_pca

prostate_pca %>%
  tidy("pcs") %>% 
  ggplot(aes(x = PC, y = percent)) +
  geom_col() +
  theme_bw()

prostate_pca %>% tidy("samples")

prostate_pca_aug <- prostate_pca %>% augment(prostate_for_pca)
prostate_pca_aug

prostate_pca_aug %>% 
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, colour = ifelse(Status==3,"Death from prostate cancer",ifelse(Status ==0 ,"Alive", "Death from other causes")))) +
  geom_point()+
  labs(colour = "Status")

prostate_k_org <- prostate_pca_aug %>%
  select(-Date_on_study, - Patient_ID, -Dataset, -Status)%>%
  kmeans(centers = 3)
prostate_k_org

prostate_pca_aug_k_org <- prostate_k_org %>%
  augment(prostate_pca_aug) %>% 
  rename(cluster_org = .cluster)
prostate_pca_aug_k_org

prostate_k_pca <- prostate_pca_aug_k_org %>%
  select(.fittedPC1, .fittedPC2) %>%
  kmeans(centers = 3)
prostate_k_pca

prostate_pca_aug_k_org_pca <- prostate_k_pca %>%
  augment(prostate_pca_aug_k_org) %>% 
  rename(cluster_pca = .cluster)
prostate_pca_aug_k_org_pca


pl1 <- prostate_pca_aug_k_org_pca %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, colour = ifelse(Status==3,"Death from prostate cancer",ifelse(Status ==0 ,"Alive", "Death from other causes")))) +
  geom_point() +
  theme(legend.position = "bottom")

pl2 <- prostate_pca_aug_k_org_pca %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, colour = cluster_org)) +
  geom_point() +
  theme(legend.position = "bottom")

pl3 <- prostate_pca_aug_k_org_pca %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, colour = cluster_pca)) +
  geom_point() +
  theme(legend.position = "bottom")
pl1
pl2
pl3

# Visualise data
# ------------------------------------------------------------------------------
corr_matrix <- ggcorrplot(prostate_one_hot_corr, hc.order = TRUE, type = "lower", title = "Correlation matrix", tl.cex = 4, 
                          lab = TRUE, lab_size = 1)


# Write data
# ------------------------------------------------------------------------------
# write_tsv(...)
ggsave("results/05_corr_matrix.png", corr_matrix)
