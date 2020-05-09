# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse)
library(reshape2)
library(broom)
library(patchwork)

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
prostate_one_hot <- read_tsv(file = "data/03_prostate_and_tcga_joined.tsv")

# Wrangle data
# ------------------------------------------------------------------------------
prostate_for_pca <- prostate_one_hot %>% 
  select(-contains("status_"))

summary(prostate_one_hot)

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}


# correlation matrix on numeric values, on dataset 0, 
# (otherwise to many NA's corrupting the table)
prostate_one_hot_corr <- prostate_one_hot %>%
  filter(., dataset == "0") %>% 
  select(-c(date_on_study, patient_id, dataset, 
            sample_id, gleason_score, primary_pattern) ) %>% 
  cor(.) %>% 
  get_lower_tri(.) %>% 
  melt(data = ., value.name = "value")  

prostate_one_hot_corr <- prostate_one_hot_corr %>% 
  mutate(value =  format(round(value, 2), nsmall = 2)) %>% 
  mutate(value = as.numeric(value))


# Model data
# ------------------------------------------------------------------------------
#PCA 
#prostate_pca <- prostate_for_pca %>%
#  select(-c(date_on_study, patient_id, dataset, cat_status)) %>%
#  prcomp(center = TRUE, scale = TRUE)  #not working 
#
prostate_pca <- prostate_for_pca %>% select(-c(sample_id, primary_pattern, 
                               gleason_score, date_on_study, 
                               patient_id, dataset, cat_status)) 


prostate_pca %>% 
  prcomp(center = TRUE, scale = TRUE)

prostate_pca %>% select_if(function(x) any(is.na(x))) %>% view()


apply(prostate_pca, 2, any(is.na(.)))
  

  
  
  prostate_pca %>%
  tidy("pcs") %>% 
  ggplot(aes(x = PC, y = percent)) +
  geom_col() +
  theme_bw()

prostate_pca %>% tidy("samples") # ?????????????????

prostate_pca_aug <- prostate_pca %>% augment(prostate_for_pca)

prostate_pca_aug %>% 
  ggplot(aes(x = .fittedPC1, 
             y = .fittedPC2, 
             colour = ifelse(cat_status == 1,"Death from prostate cancer",
                             ifelse(cat_status == 0 ,"Alive", "Death from other causes")))) +
  geom_point() +
  labs(colour = "Status")

#first clustering with kmeans based on our variables
prostate_k_org <- prostate_pca_aug %>%
  select(-c(date_on_study, patient_id, dataset, cat_status)) %>%
  kmeans(centers = 3)

prostate_pca_aug_k_org <- prostate_k_org %>%
  augment(prostate_pca_aug) %>% 
  rename(cluster_org = .cluster)

#second clustering with kmeans based on .fittedpca
prostate_k_pca <- prostate_pca_aug_k_org %>%
  select(.fittedPC1, .fittedPC2) %>%
  kmeans(centers = 3)

prostate_pca_aug_k_org_pca <- prostate_k_pca %>%
  augment(prostate_pca_aug_k_org) %>% 
  rename(cluster_pca = .cluster)

#plotting with the real division
pl1 <- prostate_pca_aug_k_org_pca %>%
  ggplot(aes(x = .fittedPC1, 
             y = .fittedPC2, 
             colour = ifelse(cat_status == 1,"Death from prostate cancer",
                             ifelse(cat_status == 0 ,"Alive", "Death from other causes")))) +
  geom_point() +
  theme(legend.position = "bottom")+
  labs(colour = "Status")

#plotting first clustering with kmeans based on our variables
pl2 <- prostate_pca_aug_k_org_pca %>%
  ggplot(aes(x = .fittedPC1, 
             y = .fittedPC2, 
             colour = cluster_org)) +
  geom_point() +
  theme(legend.position = "bottom")+
  labs(colour = "Status")

#plotting second clustering with kmeans based on .fittedpca
pl3 <- prostate_pca_aug_k_org_pca %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, colour = cluster_pca)) +
  geom_point() +
  theme(legend.position = "bottom")+
  labs(colour = "Status")

pl1 + pl2 + pl3

#which clustering gives the best division of data?
prostate_pca_aug_k_org_pca <- prostate_pca_aug_k_org_pca %>%
  mutate(cat_status = case_when(cat_status == 0 ~ "Alive",
                                  cat_status == 2 ~ "Death from other causes",
                                  cat_status == 1 ~ "Death from prostate cancer"
  ))

prostate_pca_aug_k_org_pca %>%
  select(cat_status, cluster_org, cluster_pca) %>%
  mutate(cluster_org = case_when(cluster_org == 1 ~ "Alive",
                                 cluster_org == 2 ~ "Death from other causes",
                                 cluster_org == 3 ~ "Death from prostate cancer"),
         cluster_pca = case_when(cluster_pca == 1 ~ "Death from prostate cancer",
                                 cluster_pca == 2 ~ "Death from other causes",
                                 cluster_pca == 3 ~ "Alive"),
         cluster_org_correct = case_when(cat_status == cluster_org ~ 1,
                                         cat_status != cluster_org ~ 0),
         cluster_pca_correct = case_when(cat_status == cluster_pca ~ 1,
                                         cat_status != cluster_pca ~ 0)) %>% 
  summarise(score_org = mean(cluster_org_correct),
            score_pca = mean(cluster_pca_correct))

# Visualise data
# ------------------------------------------------------------------------------
# corr_matrix <- ggcorrplot(prostate_one_hot_corr, 
#                           hc.order = TRUE, 
#                           type = "lower", 
#                           title = "Correlation matrix", 
#                           tl.cex = 4, 
#                           lab = TRUE, 
#                           lab_size = 1)

corr_matrix <- ggplot(data = prostate_one_hot_corr, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "gray") +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 2) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab"  , 
                       name="Correlation", na.value = 'white') +
  labs(title = "Correlation Matrix") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1), 
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())
 
# Write data
# ------------------------------------------------------------------------------
# write_tsv(...)
ggsave("results/05_corr_matrix.png", corr_matrix,
       width = 14,
       height = 7)
