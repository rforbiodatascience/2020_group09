# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
# Define functions
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
library("tidyverse")

# Define functions
# Load data
# ------------------------------------------------------------------------------
prostate_for_lr <- read_tsv(file = "02450Toolbox_R/Data/03_prostate_one_hot_status3.tsv")

y <- prostate_for_lr %>% select(months_fu)
y <- as.numeric(unlist(y))

X <- prostate_for_lr %>% select(tumour_size)
X <- as.integer(unlist(X))
# Estimate model parameters
w_est = lm(y ~ X,na.action=na.exclude)
coef1 <- w_est%>%select(coef[1])
coef2 <- w_est%>%select(coef[2])
# Plot the predictions of the model
plot(X, y, main='Linear regression', xlab="X", ylab="y")
y_est = coef1 + coef2*X
lines(X, y_est, col='red')

legend("topright", legend=c("Data", "Fitted model"), fill=c("black", "red"))
