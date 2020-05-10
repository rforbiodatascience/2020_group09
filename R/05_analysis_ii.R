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

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
prostate_data <- read_tsv(file = "data/03_prostate_and_tcga_joined.tsv")

# Wrangle data
# ------------------------------------------------------------------------------
prostate_for_pca <- prostate_data %>%
  select(-contains("status_")) %>%
  filter(dataset == "0")

prostate_for_pca <- prostate_for_pca %>% select(-c(
  date_on_study,
  patient_id, dataset
))

# Obtain components
prostate_pca <- prostate_for_pca %>%
  na.omit() %>%
  prcomp(center = TRUE, scale = TRUE)

# Calculte the variance explained by each component
variance_explained <- prostate_pca %>%
  tidy("pcs") %>%
  ggplot(aes(x = PC, y = percent)) +
  geom_col() +
  theme_bw() +
  labs(title = "Variance Explained")


prostate_pca_aug <- prostate_pca %>% augment(prostate_for_pca)

# Add the nominal values of cat status
prostate_pca_aug <- prostate_pca_aug %>%
  mutate(cat_status_nominal = case_when(
    cat_status == 0 ~ "Alive",
    cat_status == 2 ~ "Death other",
    cat_status == 1 ~ "Death prostate cancer"
  ))


prostate_pca_aug %>%
  ggplot(aes(
    x = .fittedPC1,
    y = .fittedPC2,
    colour = cat_status_nominal
  )) +
  geom_point() +
  labs(colour = "Status") +
  scale_color_colorblind()

# first clustering with kmeans based on our variables
prostate_k_org <- prostate_pca_aug %>%
  select(-cat_status_nominal) %>%
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

# plotting with the real division
pl1 <- prostate_pca_aug_k_org_pca %>%
  ggplot(aes(
    x = .fittedPC1,
    y = .fittedPC2,
    colour = cat_status_nominal
  )) +
  geom_point() +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical"
  ) +
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
  theme(legend.position = "bottom", legend.direction = "vertical") +
  labs(
    x = "PC1",
    y = "PC2",
    colour = "Cluster number",
    subtitle = "Clustering with Kmeans based on original variables",
    title = "Clustering"
  ) +
  scale_color_colorblind()

# plotting second clustering with kmeans based on .fittedpca
pl3 <- prostate_pca_aug_k_org_pca %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, colour = cluster_pca)) +
  geom_point() +
  theme(legend.position = "bottom", legend.direction = "vertical") +
  labs(
    x = "PC1",
    y = "PC2",
    colour = "Cluster number",
    subtitle = "Clustering with Kmeans based on principal components"
  ) +
  scale_color_colorblind()

clustering <- pl1 + pl2 + pl3

# which clustering gives the best division of data?
prostate_pca_aug_k_org_pca <- prostate_pca_aug_k_org_pca %>%
  mutate(cat_status = case_when(
    cat_status == 0 ~ "Alive",
    cat_status == 2 ~ "Death from other causes",
    cat_status == 1 ~ "Death from prostate cancer"
  ))

prostate_pca_aug_k_org_pca %>%
  select(cat_status, cluster_org, cluster_pca) %>%
  mutate(
    cluster_org = case_when(
      cluster_org == 1 ~ "Alive",
      cluster_org == 2 ~ "Death from other causes",
      cluster_org == 3 ~ "Death from prostate cancer"
    ),
    cluster_pca = case_when(
      cluster_pca == 2 ~ "Death from prostate cancer",
      cluster_pca == 3 ~ "Death from other causes",
      cluster_pca == 1 ~ "Alive"
    ),
    cluster_org_correct = case_when(
      cat_status == cluster_org ~ 1,
      cat_status != cluster_org ~ 0
    ),
    cluster_pca_correct = case_when(
      cat_status == cluster_pca ~ 1,
      cat_status != cluster_pca ~ 0
    )
  ) %>%
  summarise(
    score_org = mean(cluster_org_correct),
    score_pca = mean(cluster_pca_correct)
  )


# 3d plot
# library(plotly)
# plot_ly(x=prostate_pca_aug_k_org_pca$.fittedPC1, y=prostate_pca_aug_k_org_pca$.fittedPC2,
#         z=prostate_pca_aug_k_org_pca$.fittedPC3, type="scatter3d", mode="markers", color=prostate_pca_aug_k_org_pca$cat_status)

# Write data
# ------------------------------------------------------------------------------
# write_tsv(...)

ggsave("results/05_pca_components.png", variance_explained,
  width = 14,
  height = 7
)
ggsave("results/05_pca_clustering.png", clustering,
  width = 14,
  height = 7
)
