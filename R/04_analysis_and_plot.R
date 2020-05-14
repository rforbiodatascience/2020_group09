# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(reshape2)
library(cowplot)
require(gridExtra)
library(broom)
library(patchwork)
library(styler)

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

# selecting the continuous variables from the first dataset
data_to_plot <- prostate_ds_only %>%
  select(
    months_fu, age, weight_index,
    sbp, dbp, serum_hg, tumour_size, ap
  )

# pivoting the data in order to plot the survival (months_fu) vs
# every other varible.
data_to_plot_long <- prostate_ds_only %>%
  select(
    months_fu, age, weight_index,
    sbp, dbp, serum_hg, tumour_size, ap, cat_status
  ) %>%
  pivot_longer(
    cols = -c("cat_status", "months_fu"),
    names_to = "vars",
    values_to = "value"
  )

# correlation matrix on numeric values, on dataset 0,
data_to_corr <- prostate_ds_only %>%
  select(-c(date_on_study, patient_id, dataset, cat_status_nominal)) %>%
  cor(.) %>%
  get_lower_tri(.) %>%
  melt(data = ., value.name = "value") %>%
  mutate(value = format(round(value, 2))) %>%
  mutate(value = as.numeric(value))

# Data for linear model
prostate_data_mdl <- prostate_ds_only %>%
  group_by(stage) %>%
  nest() %>%
  mutate(mdls = map(data, get_mdls)) # get_mdls is a function that creates linear model

prostate_data_mdl_tidy <- prostate_data_mdl %>%
  mutate(tidy_col = map(mdls, tidy, conf.int = T)) %>%
  unnest(tidy_col)



# Visualise data
# ------------------------------------------------------------------------------

plot_list <- vector(mode = "list", length = length(data_to_plot))

for (i in 1:length(data_to_plot)) {
  
  col <- data_to_plot[[i]]
  x_lab <- colnames(data_to_plot)[i]
  
  barplot_i <- ggplot(data = data_to_plot, mapping = aes(x = col)) +
    geom_histogram(aes(y = ..density..),
      binwidth = function(x) 2 * IQR(x) / length(x)^(1 / 3)
    ) +
    geom_density(color = "red") +
    labs(x = x_lab) +
    theme(axis.title = element_text(size = 16))
  
  boxplot_i <- ggplot(data = data_to_plot, mapping = aes(y = col)) +
    geom_boxplot() +
    labs(x = x_lab) +
    theme(axis.title = element_text(size = 16))

  png_title <- ggdraw() +
    draw_label(paste("Histogram and boxplot of ", x_lab, sep = ""),
      size = 22
    )

  plot_row <- plot_grid(barplot_i, boxplot_i, ncol = 2)
  histogram_and_box_plot <- plot_grid(png_title,
    plot_row,
    ncol = 1, rel_heights = c(0.1, 1)
  )


  plot_list[[i]] <- histogram_and_box_plot
  names(plot_list)[i] <- paste("histogram_and_boxplot_of_", x_lab, sep = "")
}

# plotting the survival over every other variable. We coloured the points based on the the patient
# status: dead for prostate cancer or all the others
months_vs_all <- data_to_plot_long %>%
  ggplot(aes(
    x = value, y = months_fu,
    colour = ifelse(cat_status == 1, "Death from prostate cancer",
      ifelse(cat_status == 0, "Alive", "Death from other causes")
    )
  )) +
  geom_point() +
  facet_wrap(~vars, nrow = 5, scales = "free") +
  theme(strip.text.x = element_text(size = 14)) +
  labs(
    x = "",
    colour = "Status",
    title = "Months of follow-up plotted over the other continous variables"
  ) +
  theme(
    plot.title = element_text(size = 22),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 14),
  ) +
  scale_color_colorblind()

# Correlation matrix
corr_matrix <- ggplot(data = data_to_corr, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "gray") +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  scale_fill_gradient2(
    low = "#0072B2", high = "#D55E00", mid = "white",
    midpoint = 0, limit = c(-1, 1), space = "Lab",
    name = "Correlation", na.value = "white"
  ) +
  # labs(title = "Correlation Matrix") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45, vjust = 1,
      size = 10, hjust = 1
    ),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(size = 22)
  )

#Plot months follow up vs age in both datasets (in different grids)
months_fu_vs_age <- prostate_data %>%
  ggplot(aes(y = months_fu, x = age)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_point(aes(colour = cat_status_nominal), size = 2) +
  ggtitle("Age vs Months of follow-up on both datasets") +
  ylab("Months of follow-up") +
  xlab("Age") +
  facet_grid(rows = vars(dataset)) +
  #ylim(0, max_common_MF)+
  labs(color = "Status")+
  scale_color_colorblind()

# Plot months of follow up vs size in both datasets (mixed) 
months_fu_vs_size <- prostate_data %>%
  filter(dataset == 0) %>%
  ggplot(mapping = aes(y = months_fu, x = tumour_size)) +
  geom_smooth(method = "lm") +
  geom_point(aes(colour = cat_status_nominal), size = 2) +
  ggtitle("Tumour size vs Months of follow-up") +
  ylab("Months of follow-up") +
  xlab("Tumour size")+
  labs(color = "Status")+
  scale_color_colorblind()

# Linear model
months_fu_vs_size_lm <- prostate_data_mdl_tidy %>%
  filter(term == "tumour_size") %>%
  ggplot(aes(
    x = estimate,
    y = stage
  )) +
  geom_point() +
  geom_errorbar(aes(
    xmin = conf.low,
    xmax = conf.high,
    width = 0.1
  )) +
  scale_y_continuous("Tumour stage",
                     breaks = c(4, 3),
                     limits = c(2.5, 4.5),
  ) +
  labs(
    y = " ",
    title = str_c(
      "Estimated increase of months of FU ",
      "per\none squared centimeter increase of tumor size"
    )
  )



months_fu_vs_size_double_plot <- months_fu_vs_size_lm +
  months_fu_vs_size +
  facet_grid(rows = vars(desc(stage)))


# Write data
# ------------------------------------------------------------------------------
ggsave("results/04_months_fu_vs_others.png", months_vs_all,
  width = 14,
  height = 7
)

for (i in 1:length(plot_list)) {
  plot_to_save <- plot_list[[i]]
  plot_name <- names(plot_list)[i]

  path <- sprintf("results/04_%s.png", plot_name)
  ggsave(path, plot_to_save,
    width = 14,
    height = 7
  )
}

ggsave("results/04_corr_matrix.png", corr_matrix,
  width = 14,
  height = 7
)

ggsave(
  filename = "results/04_months_fu_vs_age.png",
  plot = months_fu_vs_age,
  width = 14,
  height = 7
)

ggsave(
  filename = "results/04_months_fu_vs_tumour_size.png",
  plot = months_fu_vs_size,
  width = 14,
  height = 7
)

ggsave(
  filename = "results/04_months_fu_vs_tumour_size_stage.png",
  plot = months_fu_vs_size_double_plot,
  width = 14,
  height = 7
)
