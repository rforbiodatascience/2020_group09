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
prostate_for_lr <- read_tsv(file = "data/03_prostate_one_hot_status3.tsv")

y <- prostate_for_lr %>% select(months_fu)
X <- prostate_for_lr %>% select(age)
# Estimate model parameters
w_est = lm(y ~ X);

# Plot the predictions of the model
plot(X, y, main='Linear regression', xlab="X", ylab="y");
y_est = w_est$coef[1] +w_est$coef[2]*X;
lines(X, y_est, col='red');
y_true = w0+w1*X;
lines(X, y_true, col='green');
legend("topleft", legend=c("Data", "Fitted model", "True model"), fill=c("black", "red", "green"))
