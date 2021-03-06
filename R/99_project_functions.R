# Define project functions
# ------------------------------------------------------------------------------

# one hot encoder function that transforms a factor to binary 
# with a column for each factor level.
one_hot_encoder <- function(dataset, prefix, colname){
  dataset_one_hot_encoded <- dataset %>%
                              mutate(count = 1) %>%
                              pivot_wider(names_from = (colname), 
                                          values_from = count, 
                              names_prefix = prefix, 
                              values_fill = list(count = 0)) 
  return(dataset_one_hot_encoded)
}


# ----------------------------------------------------------------------------
# function to create a linear model

get_mdls <- function(dataset) {
  return(lm(months_fu ~ tumour_size, data = dataset))
}

# ---------------------------------------------------------------------------
# Get lower triangle of the correlation matrix

get_lower_tri<-function(data){
  data[lower.tri(data)] <- NA
  return(data)
}
