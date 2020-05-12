# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse)
library(broom)
library(patchwork)
library(ggthemes)
library(styler)
library(clusteval)

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
prostate_data <- read_tsv(file = "data/03_prostate_and_tcga_joined.tsv")

# Wrangle data
# ------------------------------------------------------------------------------
prostate_ds_only <- prostate_data %>%
  select(-starts_with("status_")) %>%
  filter(dataset == "0")

prostate_for_pca <- prostate_ds_only %>% 
  select(-c(date_on_study, patient_id, dataset, cat_status_nominal))

# Obtain components
prostate_pca <- prostate_for_pca %>%
  na.omit() %>%
  prcomp(center = TRUE, scale = TRUE)

# Calculate the variance explained by each component
variance_explained <- prostate_pca %>% 
  tidy("pcs")

variance_explained_2PC <- variance_explained %>% 
  filter(PC == 2) %>% 
  select(cumulative)

prostate_pca_aug <- prostate_pca %>% 
  augment(prostate_ds_only)


# first clustering with kmeans based on our variables
prostate_k_org <- prostate_pca_aug %>%
  select(-c(date_on_study, patient_id, dataset, cat_status_nominal, cat_status)) %>%
  kmeans(centers = 3)

prostate_pca_aug_k_org <- prostate_k_org %>%
  augment(prostate_pca_aug) %>%
  rename(cluster_org = .cluster)

# second clustering with kmeans based on .fittedpca
prostate_k_pca <- prostate_pca_aug_k_org %>%
  select(.fittedPC1, .fittedPC2) %>%
  kmeans(centers = 3)

prostate_pca_aug_k_org_pca <- prostate_k_pca %>%
  augment(prostate_pca_aug_k_org) %>%
  rename(cluster_pca = .cluster)


# Visualise data
# ------------------------------------------------------------------------------

#visualisation of the scree plot
variance_explained_plot <- variance_explained %>%
  ggplot(aes(x = PC, y = percent)) +
  geom_col() +
  theme_bw() +
  labs(title = "Variance Explained")

# plotting of the first two principal components
prostate_pca_plot <- prostate_pca_aug %>%
  ggplot(aes(
    x = .fittedPC1,
    y = .fittedPC2,
    colour = cat_status_nominal
  )) +
  geom_point() +
  labs(colour = "Status", title = "PC1 vs PC2") +
  scale_color_colorblind() 

# plotting with the real division
pl1 <- prostate_pca_aug_k_org_pca %>%
  ggplot(aes(
    x = .fittedPC1,
    y = .fittedPC2,
    colour = cat_status_nominal
  )) +
  geom_point() +
  theme(legend.position = "bottom",
        legend.direction = "vertical", 
        plot.subtitle = element_text(size = 16), 
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 16)) +
  labs(
    x = "PC1",
    y = "PC2",
    colour = "Status",
    subtitle = "Real division"
  ) +
  scale_color_colorblind()

# plotting first clustering with kmeans based on our variables
pl2 <- prostate_pca_aug_k_org_pca %>%
  ggplot(aes(
    x = .fittedPC1,
    y = .fittedPC2,
    colour = cluster_org
  )) +
  geom_point() +
  theme(legend.position = "bottom", 
        legend.direction = "vertical",
        plot.title = element_text(size = 22),
        plot.subtitle = element_text(size = 16), 
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 16))  +
  labs(x = "PC1",
       y = "PC2",
       colour = "Cluster number",
       subtitle = "Clustering with Kmeans\nbased on original variables",
       title = "Clustering") +
  scale_color_colorblind()

# plotting second clustering with kmeans based on .fittedpca
pl3 <- prostate_pca_aug_k_org_pca %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, colour = cluster_pca)) +
  geom_point() +
  theme(legend.position = "bottom", 
        legend.direction = "vertical",
        plot.title = element_text(size = 22),
        plot.subtitle = element_text(size = 16), 
        legend.text = element_text(size = 14), 
        legend.title = element_text(size = 16)) +
  labs(
    x = "PC1",
    y = "PC2",
    colour = "Cluster number",
    subtitle = "Clustering with Kmeans\nbased on principal components"
  ) +
  scale_color_colorblind()

clustering <- pl1 + pl2 + pl3

# which clustering gives the most accurate division of data?

# Check the similarity between the clustering 
# and the original data classification
# 0 is no similarity, 1 is exactly the same classification
# External library, clusterval

sim_org <- jaccard(prostate_pca_aug_k_org_pca$cat_status,
                   prostate_pca_aug_k_org_pca$cluster_org) 

sim_pca <- jaccard(prostate_pca_aug_k_org_pca$cat_status, 
                   prostate_pca_aug_k_org_pca$cluster_pca)

# 3d plot
# library(plotly)
# plot_ly(x=prostate_pca_aug_k_org_pca$.fittedPC1, y=prostate_pca_aug_k_org_pca$.fittedPC2,
#         z=prostate_pca_aug_k_org_pca$.fittedPC3, type="scatter3d", mode="markers", color=prostate_pca_aug_k_org_pca$cat_status)

# Write data
# ------------------------------------------------------------------------------

ggsave("results/05_pca_components.png", variance_explained_plot,
       width = 14,
       height = 7
)

ggsave("results/05_pca_clustering.png", clustering,
       width = 14,
       height = 7
)

ggsave("results/05_pca_status.png", prostate_pca_plot,
       width = 8,
       height = 4)
