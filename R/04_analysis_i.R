# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library("tidyverse")
library("ggplot2")
require("gridExtra")

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
# data for initial visualization and data exploration (status still as factor)
prostate_data <- read_tsv(file = "data/02_prostate_data_clean.tsv")

# Wrangle data
# ------------------------------------------------------------------------------
#this is not working for unknown reasons - just returns NA
prostate_data_status_in_numbers <- prostate_data %>%
  mutate(Status = as.integer(Status)-1)

 

# Model data
# ------------------------------------------------------------------------------


# Visualise data
# ------------------------------------------------------------------------------

data_to_plot <- prostate_data %>% 
  dplyr::select(c(Months_FU, Age, Weight_index, SBP, DBP, Serum_HG, Tumour_size, AP))

plot_list <- vector("list", length = length(colnames(data_to_plot)))
for (i in 1:length(plot_list)) {
  plot_list[[i]] <- list("boxplot" = NULL, "barplot" = NULL)
}

j <- 1

for (i in data_to_plot) {
  
  barplot_i <- ggplot(data = data_to_plot, mapping = aes(x = i)) +
    geom_histogram(aes(y=..density..)) +
    geom_density(color = "red") +
    labs(x = colnames(data_to_plot)[j])
  
  boxplot_i <- ggplot(data = data_to_plot, mapping = aes(y = i)) +
    geom_boxplot() +
    labs(x = colnames(data_to_plot)[j])
  
  grid.arrange(barplot_i, boxplot_i, ncol=2) 
  
  j <- j + 1
}

#not working since the status is NA and using base R
plot(data_to_plot, pch = 16, col = ifelse(prostate_data_status_in_numbers$Status == 0, "black", "red"))


# Write data
# ------------------------------------------------------------------------------
# write_tsv(...)

