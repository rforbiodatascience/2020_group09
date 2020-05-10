# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse)
library(broom)
library(patchwork)
library(styler)

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
prostate_for_lr <- read_tsv(file = "data/03_prostate_and_tcga_joined.tsv")

# Wranglisng data
# ------------------------------------------------------------------------------

max_common_MF <- prostate_for_lr %>%
  filter(dataset == 0) %>%
  select(months_fu) %>%
  max()

# Visualize data
# ------------------------------------------------------------------------------

months_fu_vs_age <- prostate_for_lr %>%
  ggplot(aes(y = months_fu, x = age)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_point(aes(colour = as_factor(cat_status)), size = 2) +
  ggtitle("Age vs Months of follow-up") +
  ylab("Months of follow-up") +
  xlab("Age") +
  scale_color_discrete(name = "Status",
                       labels = c("Alive", "Dead of prostate cancer",
                                  "Dead of other causes")) +
  facet_grid(rows = vars(dataset)) +
  ylim(0, max_common_MF)

months_fu_vs_size <- prostate_for_lr %>%
  filter(dataset == 0) %>%
  ggplot(mapping = aes(y = months_fu, x = tumour_size)) +
  geom_smooth(method = "lm") +
  geom_point(aes(colour = as_factor(cat_status)), size = 2) +
  ggtitle("Tumour size vs Months of follow-up") +
  ylab("Months of follow-up") +
  xlab("Tumour size") +
  scale_color_discrete(name = "Status",
                       labels = c("Alive",
                                  "Dead of prostate cancer", 
                                  "Dead of other causes"))

months_fu_vs_age + months_fu_vs_size

prostate_data_mdl <- prostate_for_lr %>%
  filter(dataset == 0) %>%
  group_by(stage) %>%
  nest() %>%
  mutate(mdls = map(data, get_mdls)) # get_mdls is a function that creates linear model

prostate_data_mdl_tidy <- prostate_data_mdl %>%
  mutate(tidy_col = map(mdls, tidy, conf.int = T)) %>%
  unnest(tidy_col)

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
    # expand = c(0,0)
  ) +
  # sets the origin to zero
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




mdl_continuous_variables <- prostate_for_lr %>%
  filter(dataset == 0) %>%
  lm(
    formula = months_fu ~ age +
      weight_index +
      sbp +
      dbp +
      serum_hg +
      tumour_size +
      ap,
    data = .
  )

plot(mdl_continuous_variables)



# Write data
# ------------------------------------------------------------------------------

ggsave(
  filename = "results/06_months_fu_vs_age.png",
  plot = months_fu_vs_age,
  width = 14,
  height = 7
)
ggsave(
  filename = "results/06_months_fu_vs_tumour_size.png",
  plot = months_fu_vs_size,
  width = 14,
  height = 7
)
ggsave(
  filename = "results/06_months_fu_vs_tumour_size_stage.png",
  plot = months_fu_vs_size_double_plot,
  width = 14,
  height = 7
)
