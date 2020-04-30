# Define project functions
# ------------------------------------------------------------------------------

one_hot_encoder <- function(col_name, prefix){
  group_by(col_name) %>%
  mutate(count = 1) %>%
  pivot_wider(names_from = col_name, 
              values_from = count, 
              names_prefix = prefix, 
              values_fill = list(count = 0)) 
}

foo <- function(x){
  return(2*x)
}
bar <- function(x){
  return(x^2)
}
