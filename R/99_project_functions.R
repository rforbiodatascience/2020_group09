# Define project functions
# ------------------------------------------------------------------------------

one_hot_encoder <- function(dataset, prefix, ...){
  dataset_one_hot_encoded <- dataset %>%
                              group_by(...) %>%
                              mutate(count = 1) %>%
                              pivot_wider(names_from = ..., 
                                          values_from = count, 
                              names_prefix = prefix, 
                              values_fill = list(count = 0)) 
  return(dataset_one_hot_encoded)
}

# function to change factor columns into new columns for each level denoted 
# by 0,1, 0 for abscent 1 for present - one hot encoding.  
one_hot_encoder <- function(col_name, prefix){
  group_by(col_name) %>%
  mutate(count = 1) %>%
  pivot_wider(names_from = col_name, 
              values_from = count, 
              names_prefix = prefix, 
              values_fill = list(count = 0)) 
}

# ----------------------------------------------------------------------------
# function to create a linear model

get_mdls <- function(dataset) {
  return(lm(months_fu ~ tumour_size, data = dataset))
}