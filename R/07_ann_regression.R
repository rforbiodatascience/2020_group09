# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse)
library(devtools)
library(keras)
library(modelr)
library(styler)


# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
joined_data <- read_tsv(file = "data/03_prostate_and_tcga_joined.tsv")

regr_data <- joined_data %>%
  select(
    months_fu, status_alive, age, sg, bone_metastases,
    status_dead_prostatic_ca, status_dead_other, dataset
  )
#
# Prepare data
# ------------------------------------------------------------------------------
## select 90% of the sample size for training-testing
smp_size <- floor(0.9 * nrow(regr_data))
set.seed(42)
traintest_ind <- sample(seq_len(nrow(regr_data)), size = smp_size)

traintest <- regr_data %>%
  filter(row_number() %in% traintest_ind)
eval <- regr_data %>%
  filter(!(row_number() %in% traintest_ind))

eval_y <- eval %>%
  pull(months_fu) %>%
  as.integer()

eval_x <- eval %>%
  select(-c(months_fu)) %>%
  as.matrix()

# Split train and test (80%)
smp_size <- floor(0.8 * nrow(traintest))
set.seed(42)
train_ind <- sample(seq_len(nrow(traintest)), size = smp_size)

train <- traintest %>%
  filter(row_number() %in% train_ind)
test <- traintest %>%
  filter(!(row_number() %in% train_ind))

train_y <- train %>%
  pull(months_fu) %>%
  as.integer()

train_x <- train %>%
  select(-c(months_fu)) %>%
  as.matrix()

test_y <- test %>%
  pull(months_fu) %>%
  as.integer()

test_x <- test %>%
  select(-c(months_fu)) %>%
  as.matrix()


# Define ANN model
# ------------------------------------------------------------------------------

# Set hyperparameters
n_hidden_1 <- 128
h1_activate <- "relu"
drop_out_1 <- 0.4
n_output <- 1
o_ativate <- "relu"
n_epochs <- 100
batch_size <- 50
loss_func <- "mean_squared_error"
learn_rate <- 0.005

# Set architecture
model <- keras_model_sequential() %>%
  layer_dense(
    units = n_hidden_1,
    activation = h1_activate, input_shape = 7,
    kernel_regularizer = regularizer_l2(0.001)
  ) %>%
  layer_dropout(rate = drop_out_1) %>%
  layer_dense(units = n_output, activation = o_ativate)

# Compile model
model %>%
  compile(
    loss = loss_func,
    optimizer = optimizer_rmsprop(lr = learn_rate),
    metrics = c("mae")
  )

# View model
model %>%
  summary() %>%
  print()

# Train model
# ------------------------------------------------------------------------------
# Create a split of 80% in the train-test data to get train and test
history <- model %>%
  fit(
    x = train_x,
    y = train_y,
    epochs = n_epochs,
    batch_size = batch_size,
    # validation_split = 0.8) #Makes a split
    validation_data = list(test_x, test_y)
  ) # Uses a previous split

# Evaluate model
# ------------------------------------------------------------------------------

# Calculate performance on training data
y_train_true <- train_y
y_train_pred <- model %>%
  predict(train_x) %>%
  as.vector()

perf_test <- model %>% evaluate(test_x, test_y)
mae_test <- perf_test %>%
  pluck("mean_absolute_error") %>%
  round(3) #* 100

perf_train <- model %>% evaluate(train_x, train_y)
mae_train <- perf_train %>%
  pluck("mean_absolute_error") %>%
  round(3) # * 100


# Visualise model performance
# ------------------------------------------------------------------------------


# Save model
# ------------------------------------------------------------------------------
save_model_hdf5(
  object = model,
  filepath = "models/07_ann_regression.h5"
)

# ------------------------------------------------------------------------------
# Predictions
predictions <- model %>% predict(eval_x)
y_eval_true <- eval_y
y_eval_pred <- model %>%
  predict(eval_x) %>%
  as.vector()
pcc_eval <- round(cor(y_eval_pred, y_eval_true, method = "pearson"), 3)
