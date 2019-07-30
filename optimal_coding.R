# This is an example of how we can unsupervisingly recode a categorical variable.
# Some sort of automatic feature re-engineering. Isn't it something in the same
# spirit as deep learning?

optimal_coding <- function(df, f = function(x) chisq.test(table(x))$p.value) {
  require(partitions)
  unique_val <- lapply(df, unique) # list of 2
  sets <- lapply(unique_val, function(x) data.frame(unclass(setparts(length(x))))[, -1])
  cmbns <- expand.grid(sapply(sets, seq_along))
  
  recode <- function(x) { # depends on unique_val, sets, cmbns and df
    for(i in 1:2)
      df[[i]] <- setNames(sets[[c(i, cmbns[x, i])]], unique_val[[i]])[df[[i]]]
    df
  }
  
  extract <- function(x, list) list[[x]]
  
  which_min <- function(x) which(x == min(x)) # which.min() returns only 1 value
  
  retrieve_combinations <- function(x) { # depends on cmbns, extract(), sets, unique_val and df
    x %>% 
      `[`(cmbns, ., ) %>% 
      map2(1:2, ., c) %>% 
      lapply(extract, sets) %>% 
      map2(unique_val, setNames) %>% 
      setNames(names(df))
  }
  
  1:nrow(cmbns) %>% 
    sapply(function(x) f(recode(x))) %>% 
    which_min() %>% # there may be several optimal combinations
    lapply(retrieve_combinations)
}


# Let's try it:
optimal_coding(na.exclude(mutate_all(mics_survey$variables[, c("melevel", "HH7")], as.character)))
