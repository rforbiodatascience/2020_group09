# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library("tidyverse")
library("devtools")
library(keras)
library("modelr")

# Install keras
# ------------------------------------------------------------------------------
# install.packages("devtools")
# library("devtools")
# install_github("rstudio/keras")
# Would you like to install miniconda? Y
# library(keras)
# install_keras(tensorflow = "1.13.1")

# Define functions
# ------------------------------------------------------------------------------
source(file = 'R/99_project_functions.R')

# Load data
# ------------------------------------------------------------------------------
joined_data = read_tsv(file = "data/03_prostate_and_tcga_joined.tsv")

regr_data <- joined_data %>% 
  select(months_fu, status_alive,  age, sg, bone_metastases,
         status_dead_prostatic_ca, status_dead_other, dataset)
# 
# Prepare data
# ------------------------------------------------------------------------------
## select 80% of the sample size for training
smp_size <- floor(0.9 * nrow(regr_data))
set.seed(42)
train_ind <- sample(seq_len(nrow(regr_data)), size = smp_size)

train <- regr_data %>%
  filter(row_number() %in% train_ind)
test <- regr_data %>% 
  filter(!(row_number() %in% train_ind))

train_y = train %>% 
  pull(months_fu)%>% 
  as.integer()

train_x <- train  %>% 
  select(-c(months_fu))%>% 
  as.matrix() 

test_y <- test  %>% 
  pull(months_fu)%>% 
  as.integer()

test_x <- test %>% 
  select(-c(months_fu))%>% 
  as.matrix()  

# Define ANN model
# ------------------------------------------------------------------------------

# Set hyperparameters
n_hidden_1 = 128
h1_activate = 'relu'
drop_out_1 = 0.4
#n_hidden_2 = 128
#h2_activate = 'relu'
# drop_out_2 = 0.3
# n_hidden_3 = 45
# h3_activate = 'relu'
# drop_out_3 = 0.2
# n_hidden_4 = 20
# h4_activate = 'relu'
# drop_out_4 = 0.1
n_output   = 1
o_ativate  = 'relu'
n_epochs = 100
batch_size = 50
loss_func = 'mean_squared_error'
learn_rate = 0.005

# Set architecture
model = keras_model_sequential() %>% 
  layer_dense(units = n_hidden_1, 
              activation = h1_activate, input_shape = 7,
              kernel_regularizer =  regularizer_l2(0.001)) %>% 
  layer_dropout(rate = drop_out_1) %>% 
  # layer_dense(units = n_hidden_2, activation = h2_activate) %>%
  # layer_dropout(rate = drop_out_2) %>%
  # layer_dense(units = n_hidden_3, activation = h3_activate) %>%
  # layer_dropout(rate = drop_out_3) %>%
  # layer_dense(units = n_hidden_4, activation = h4_activate) %>%
  # layer_dropout(rate = drop_out_4) %>%
  layer_dense(units = n_output, activation = o_ativate)

# Compile model
model %>%
  compile(loss = loss_func,
          optimizer = optimizer_rmsprop(lr = learn_rate),
          metrics = c('mae')
  )

# View model
model %>% summary %>% print
#train_x
# Train model
# ------------------------------------------------------------------------------
history = model %>%
  fit(x = train_x,
      y = train_y,
      epochs = n_epochs,
      batch_size = batch_size,
      #validation_split = 0.8)
      validation_data = list(test_x, test_y))

# Evaluate model
# ------------------------------------------------------------------------------
# Calculate performance on test data
y_test_true = test_y
y_test_pred = model %>% predict(test_x) %>% as.vector
pcc_test = round(cor(y_test_pred, y_test_true, method = "pearson"), 3)

# Calculate performance on training data
y_train_true = train_y
y_train_pred = model %>% predict(train_x) %>% as.vector
perf_test

perf_test = model %>% evaluate(test_x, test_y)
mae_test = perf_test %>% pluck('mean_absolute_error') %>% round(3) #* 100

perf_train = model %>% evaluate(train_x, train_x)
mae_train = perf_train %>% pluck('mean_absolute_error') %>% round(3)# * 100
results = bind_rows(
  tibble(y_true = test_y %>%
           apply(1, function(x){ return( which(x==1) - 1) }) %>%
           factor,
         y_pred = model %>%
           predict_classes(test_x) %>%
           factor,
         Correct = ifelse(y_true == y_pred ,"yes", "no") %>%
           factor,
         data_type = 'test')
  ,
  tibble(y_true = train_y %>%
           apply(1, function(x){ return( which(x==1) - 1) }) %>%
           factor,
         y_pred = model %>%
           predict_classes(train_x) %>%
           factor,
         Correct = ifelse(y_true == y_pred ,"yes", "no") %>%
           factor,
         data_type = 'train'))
my_counts = results %>% count(y_pred, y_true, data_type)

# Visualise model performance
# ------------------------------------------------------------------------------
title = paste0('Performance of Deep Feed Forward Neural Network (',
               'Total number of model parameters = ', count_params(model), ').')
sub_title = paste0("Test Accuracy = ", acc_test, "%, n = ", nrow(X_test), ". ",
                   "Training Accuracy = ", acc_train, "%, n = ", nrow(X_train), ".")
xlab  = 'Predicted (Class assigned by Keras/TensorFlow deep FFN)'
ylab  = 'Measured (Real class, as predicted by netMHCpan-4.0)'
results %>%
  ggplot(aes(x = y_pred, y = y_true, fill = Correct)) +
  geom_jitter(pch = 21, size = 4, alpha = 0.4, colour = 'black') +
  geom_text(data = my_counts, aes(x = y_pred, y = y_true, label = n),
            size = 20, inherit.aes = FALSE) +
  xlab(xlab) +
  ylab(ylab) +
  ggtitle(label = title, subtitle = sub_title) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_color_manual(labels = c('No', 'Yes'),
                     values = c('tomato','cornflowerblue')) +
  facet_wrap(~data_type, nrow = 1)

# Save model
# ------------------------------------------------------------------------------
save_model_hdf5(object = model,
                filepath = "Models/05_peptide_model.h5")

# ------------------------------------------------------------------------------
# Predictions
predictions = model %>% predict(X_eval)
predictions
