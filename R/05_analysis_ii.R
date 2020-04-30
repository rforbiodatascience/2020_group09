# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library("tidyverse")
library("ggcorrplot")
library(broom)
library(patchwork)
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
#PCA 
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
#first clustering with kmeans based on our variables
prostate_k_org <- prostate_pca_aug %>%
  select(-Date_on_study, - Patient_ID, -Dataset, -Status)%>%
  kmeans(centers = 3)
prostate_k_org

prostate_pca_aug_k_org <- prostate_k_org %>%
  augment(prostate_pca_aug) %>% 
  rename(cluster_org = .cluster)
prostate_pca_aug_k_org
#second clustering with kmeans based on .fittedpca
prostate_k_pca <- prostate_pca_aug_k_org %>%
  select(.fittedPC1, .fittedPC2) %>%
  kmeans(centers = 3)
prostate_k_pca

prostate_pca_aug_k_org_pca <- prostate_k_pca %>%
  augment(prostate_pca_aug_k_org) %>% 
  rename(cluster_pca = .cluster)
prostate_pca_aug_k_org_pca

#plotting with the real division
pl1 <- prostate_pca_aug_k_org_pca %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, colour = ifelse(Status==3,"Death from prostate cancer",ifelse(Status ==0 ,"Alive", "Death from other causes")))) +
  geom_point() +
  theme(legend.position = "bottom")+
  labs(colour = "Status")
#plotting first clustering with kmeans based on our variables
pl2 <- prostate_pca_aug_k_org_pca %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, colour = cluster_org)) +
  geom_point() +
  theme(legend.position = "bottom")+
  labs(colour = "Status")
#plotting second clustering with kmeans based on .fittedpca
pl3 <- prostate_pca_aug_k_org_pca %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, colour = cluster_pca)) +
  geom_point() +
  theme(legend.position = "bottom")+
  labs(colour = "Status")

(pl1 + pl2 + pl3)

#which clustering gives the best division of data?
prostate_pca_aug_k_org_pca <- prostate_pca_aug_k_org_pca %>%
  mutate(Status_class = case_when(Status == 0 ~ "Alive",
                                  Status < 3 ~ "Death from other causes",
                                  Status > 3 ~ "Death from other causes",
                                  Status ==3 ~ "Death from prostate cancer"
  ))

prostate_pca_aug_k_org_pca %>%
  select(Status_class, cluster_org, cluster_pca) %>%
  mutate(cluster_org = case_when(cluster_org == 1 ~ "Alive",
                                 cluster_org == 2 ~ "Death from other causes",
                                 cluster_org == 3 ~ "Death from prostate cancer"),
         cluster_pca = case_when(cluster_pca == 1 ~ "Death from prostate cancer",
                                 cluster_pca == 2 ~ "Death from other causes",
                                 cluster_pca == 3 ~ "Alive"),
         cluster_org_correct = case_when(Status_class == cluster_org ~ 1,
                                         Status_class != cluster_org ~ 0),
         cluster_pca_correct = case_when(Status_class == cluster_pca ~ 1,
                                         Status_class != cluster_pca ~ 0)) %>% 
  summarise(score_org = mean(cluster_org_correct),
            score_pca = mean(cluster_pca_correct))
# Visualise data
# ------------------------------------------------------------------------------
corr_matrix <- ggcorrplot(prostate_one_hot_corr, hc.order = TRUE, type = "lower", title = "Correlation matrix", tl.cex = 4, 
                          lab = TRUE, lab_size = 1)


# Write data
# ------------------------------------------------------------------------------
# write_tsv(...)
ggsave("results/05_corr_matrix.png", corr_matrix)
