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

