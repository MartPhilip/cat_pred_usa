predation_pressure_info <- function(response_vector){
  predation_vector_all <- response_vector
  predation_vector_prey <-   predation_vector_all[predation_vector_all>=0.0066563318]
  predation_vector_all_summary <- c(min(predation_vector_all),max(predation_vector_all),mean(predation_vector_all),sd(predation_vector_all))
  predation_vector_prey_summary <- c(min(predation_vector_prey),max(predation_vector_prey),mean(predation_vector_prey),sd(predation_vector_prey))
  predation_data_summary <- rbind(predation_vector_all_summary,predation_vector_prey_summary)
  colnames(predation_data_summary) <- c("Min","Max","Mean","Sd")
  rownames(predation_data_summary) <- c("All","Prey")
  return(predation_data_summary)
}
