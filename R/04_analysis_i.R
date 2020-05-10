# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggthemes)
require(gridExtra)

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
# data for initial visualization and data exploration 
prostate_data <- read_tsv(file = "data/03_prostate_and_tcga_joined.tsv")

# Wrangle data
# ------------------------------------------------------------------------------
prostate_ds_only <- prostate_data %>% 
  filter(dataset == 0)

#selecting the continuous variables from the first dataset 
data_to_plot <- prostate_ds_only %>% 
  select(months_fu, age, weight_index,
         sbp, dbp, serum_hg, tumour_size, ap)

# pivoting the data in order to plot the survival (months_fu) vs
# every other varible. 
data_to_plot_long <- prostate_ds_only %>% 
  select(months_fu, age, weight_index,
         sbp, dbp, serum_hg, tumour_size, ap, cat_status) %>% 
  pivot_longer(cols = -c("cat_status","months_fu"),
               names_to = "vars",
               values_to = "value") 

# Visualise data
# ------------------------------------------------------------------------------

plot_list <- vector("list", length = length(data_to_plot))

for (i in 1:length(data_to_plot)
) {
  col <- data_to_plot[[i]]
  x_lab <- colnames(data_to_plot)[i]
  
  png_title <- sprintf("Histogram and boxplot of %s", x_lab)
  
  barplot_i <- ggplot(data = data_to_plot, mapping = aes(x = col)) +
    geom_histogram(aes(y = ..density..), 
                   binwidth = function(x)2*IQR(x)/length(x)^(1/3)) +
    geom_density(color = "red") +
    labs(x = x_lab)
  
  boxplot_i <- ggplot(data = data_to_plot, mapping = aes(y = col)) +
    geom_boxplot() +
    labs(x = x_lab)
  
  
  histogram_and_box_plot <- grid.arrange(barplot_i, 
                                         boxplot_i, ncol = 2, top = png_title) 
  
  plot_list[[i]] <- histogram_and_box_plot
  names(plot_list)[i] <- paste("histogram_and_boxplot_of_", x_lab, sep = "")
}

#plotting the survival over every other variable. We coloured the points based on the the patient
# status: dead for prostate cancer or all the others
months_vs_all <- data_to_plot_long %>%
  ggplot(aes(x = value, y = months_fu, 
             colour = ifelse(cat_status == 1, "Death from prostate cancer",
                             ifelse(cat_status == 0, "Alive", "Death from other causes") ))) +
  geom_point() +
  facet_wrap(~vars, nrow = 5, scales = "free") +
  labs(x = "", colour = "Status", title = 'Months of follow-up plotted over the other continous variables') +
  scale_color_colorblind()
 
months_vs_all

# Write data
# ------------------------------------------------------------------------------
ggsave('results/04_months_fu_vs_others.png', months_vs_all, 
       width = 14,
       height = 7)

for (i in 1:length(plot_list)) {
  
  plot_to_save <- plot_list[[i]]
  plot_name <- names(plot_list)[i]
  
  path <- sprintf("results/04_%s.png", plot_name)
  ggsave(path, plot_to_save,
         width = 14,
         height = 7)
  
}