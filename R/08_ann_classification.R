# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library(tidyverse)
library(devtools)
library(keras)
library(modelr) 
library(clusteval)

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
joined_data <- read_tsv(file = "data/03_prostate_and_tcga_joined.tsv")

class_data <- joined_data %>%
  select(-contains("status_"), -date_on_study, -patient_id) %>%
  filter(dataset == 0) %>%
  select(-dataset)
#
# Prepare data
# ------------------------------------------------------------------------------
## select 80% of the sample size for training
test_f <- 0.2
class_data <- class_data %>%
  mutate(partition = sample(x = c("train", "test"), 
                            size = nrow(.),
                            replace = TRUE,
                            prob = c(1 - test_f, test_f)))

train_y <- class_data %>%
  filter(partition == "train") %>%
  pull(cat_status) %>%
  to_categorical(3)

train_x <- class_data %>%
  filter(partition == "train") %>%
  select(-cat_status, -partition) %>%
  as.matrix()

test_y <- class_data %>%
  filter(partition == "test") %>%
  pull(cat_status) %>%
  to_categorical(3)

test_x <- class_data %>%
  filter(partition == "test") %>%
  select(-cat_status, -partition) %>%
  as.matrix()

# Define ANN model
# ------------------------------------------------------------------------------

# Set hyperparameters
n_hidden_1 <- 4
h1_activate <- "relu"
drop_out_1 <- 0.1
n_output <- 3
o_ativate <- "softmax"
n_epochs <- 200
batch_size <- 50
loss_func <- ""
learn_rate <- 0.005

# Set architecture
model <- keras_model_sequential() %>%
  layer_dense(
    units = n_hidden_1,
    activation = h1_activate, input_shape = c(24)) %>%
  layer_dense(units = n_output, activation = o_ativate)

# Compile model
model <- model %>%
  compile(
    loss = "categorical_crossentropy",
    optimizer = "adam",
    metrics = "accuracy"
  )

# View model
model %>%
  summary() %>%
  print()
model

# Train model
# ------------------------------------------------------------------------------
history <- model %>%
  fit(
    x = train_x,
    y = train_y,
    epochs = n_epochs,
    batch_size = batch_size,
    validation_split = 0.33
  )


plot_loss_acc<-plot(history,
                    method= "ggplot2",
                    smooth = getOption("keras.plot.history.smooth", FALSE))


# Evaluate model
# ------------------------------------------------------------------------------
perf_test = model %>% evaluate(test_x, test_y)
acc_test = perf_test %>% pluck('acc') %>% round(3) * 100
perf_train = model %>% evaluate(test_x, test_y)
acc_train = perf_train %>% pluck('acc') %>% round(3) * 100

y_true_test = test_y %>%
    apply(1, function(x){ return( which(x==1) - 1) }) %>%
    factor

y_pred_test = model %>%
  predict_classes(test_x) %>%
  factor(levels = c(0,1,2))

y_true_train = train_y %>%
  apply(1, function(x){ return( which(x==1) - 1) }) %>%
  factor

y_pred_train = model %>%
  predict_classes(train_x) %>%
  factor(levels = c(0,1,2))

results = bind_rows(
  tibble(y_true = y_true_test,
         y_pred = y_pred_test,
         Correct = ifelse(y_true == y_pred ,"yes", "no") %>%
           factor,
         data_type = 'test')
  ,
  tibble(y_true = y_true_train,
         y_pred = y_pred_train,
         Correct = ifelse(y_true == y_pred ,"yes", "no") %>%
           factor,
         data_type = 'train'))
my_counts = results %>% count(y_pred, y_true, data_type)

# Save model
# ------------------------------------------------------------------------------
save_model_hdf5(
  object = model,
  filepath = "models/08_classification.h5"
)


# Visualise model performance
# ------------------------------------------------------------------------------
title = paste0('Performance of Neural Network (',
               'Total number of model parameters = ', count_params(model), ').')
sub_title = paste0("Test Accuracy = ", acc_test, "%, n = ", nrow(test_x), ". ",
                   "Training Accuracy = ", acc_train, "%, n = ", nrow(train_x), ".")
xlab  = 'Predicted classes'
ylab  = 'True classes'
evaluation_classification <- results %>%
  ggplot(aes(x = y_pred, y = y_true, fill = Correct)) +
  geom_jitter(pch = 21, size = 4, alpha = 0.6, colour = 'black') +
  geom_text(data = my_counts, aes(x = y_pred, y = y_true, label = n),
            size = 20, inherit.aes = FALSE) +
  xlab(xlab) +
  ylab(ylab) +
  ggtitle(label = title, subtitle = sub_title) +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 22),
        plot.subtitle = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.title = element_text(size = 14), 
        legend.text =  element_text(size = 14)) +
  scale_color_manual(labels = c('No', 'Yes'),
                     values = c('tomato','cornflowerblue')) +
  facet_wrap(~data_type, nrow = 1)
evaluation_classification


# ------------------------------------------------------------------------------
# Predictions
predictions <- model %>% predict(train_x)
predictions

perf = model %>% evaluate(test_x, test_y)


plot_dat = class_data %>%
  filter(partition == 'test') %>%
  mutate(cat_status = factor(cat_status),
         y_test_pred = factor(predict_classes(model, test_x),levels = c(0,1,2)),
         correct = factor(ifelse(cat_status == y_test_pred, "Yes", "No")))
plot_dat %>% head(3)


# Save graph
# ------------------------------------------------------------------------------
ggsave("results/08_loss_acc.png", plot_loss_acc,
       width = 14,
       height = 7
)
ggsave("results/08_evaluation_classification.png", evaluation_classification,
       width = 14,
       height = 7
)   

