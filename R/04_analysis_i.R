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
  mutate(Status = as.integer(Status))
 

# Model data
# ------------------------------------------------------------------------------


# Visualise data
# ------------------------------------------------------------------------------

data_to_plot <- prostate_data_status_in_numbers %>% 
  dplyr::select(c(Patient_ID, Status, Months_FU, Age, Weight_index, SBP, DBP, Serum_HG, Tumour_size, AP))

plot_list <- vector("list", length = length(colnames(data_to_plot)))
for (i in 1:length(plot_list)) {
  plot_list[[i]] <- list("boxplot" = NULL, "barplot" = NULL)
}
view(data_to_plot)
j <- 3

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

#plotting the survival over every other variable. We coloured the points dividing between
#the patients dead for prostate cancer and all the others
data_to_plot_long <- data_to_plot %>%
  pivot_longer(cols = -c("Patient_ID", "Status"),
               names_to = "vars",
               values_to = "value") %>% 
  mutate(Patient_ID = factor(Patient_ID),
         Status = Status)
data_to_plot_long

data_to_plot_long %>%
  ggplot(aes(x = Patient_ID, y = value, colour = ifelse(Status == 3, "Dead for prostate cancer", "Survived or dead for other causes"))) +
  geom_point() +
  facet_wrap(~vars, nrow = 5, scales = "free_y") +
  labs(x = "", colour = "Status") +
  theme(axis.text.x = element_blank())


# Write data
# ------------------------------------------------------------------------------
# write_tsv(...)

