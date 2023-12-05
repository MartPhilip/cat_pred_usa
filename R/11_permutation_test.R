permutation_test <- function(data,num_permutations){
  num_permutations <- num_permutations
  # Observed difference in median
  observed_diff <- median(data$prediction[data$`Species type` == 'Empirical record']) -
    median(data$prediction[data$`Species type` == 'No record'])
  # Permutation test
  perm_diffs <- replicate(num_permutations, {
    # Permute Species type labels
    permuted_species <- sample(data$`Species type`)
    
    # Calculate median difference for each permutation
    perm_median_diff <- median(data$prediction[permuted_species == 'Empirical record']) -
      median(data$prediction[permuted_species == 'No record'])
    
    return(perm_median_diff)
  })
  
  # Calculate the p-value
  p_value <- length(perm_diffs[perm_diffs>observed_diff])/num_permutations
  return(p_value)
}
