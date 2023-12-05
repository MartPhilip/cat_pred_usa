computing_random_forest <- function(data,
                                  species,
                                  response_data,
                                  trait_data,
                                  phylo_data = NULL,
                                  classification, # binary or regression
                                  weight = NULL,
                                  mtry_frac = NULL,
                                  min.node.size = NULL,
                                  sample.fraction = NULL,
                                  ntrees = NULL,
                                  PEMs = NULL,
                                  cores = 1)

{
  
  if(classification) {
    
    if(is.numeric(trait_data[, response_data])) {
      
      stop("You want to do classification but your data are numeric !")
      
    }
    
  }
  
  if(is.null(phylo_data)){
    
    phylo <- FALSE
    
  }else{
    
    phylo <- TRUE
    
  }
  
  if(is.null(weight)){
    
    weight <- weight_scenarii_list(trait_data[,response_data], 0.5)
    
  }
  
  n_features <- dim(trait_data)[2] - 2
  
  formula <- stats::as.formula(paste0(response_data, " ~ .-", species))
  
  rf1 <- ranger::ranger(formula = formula,
                        data = trait_data,
                        mtry = floor(n_features / 3),
                        respect.unordered.factors = "order",
                        seed = 123,
                        case.weights = weight[[1]])
  
  default_rmse <- sqrt(rf1$prediction.error)
  
  # execute full cartesian grid search, no weights assigned in this run
  start_time <- Sys.time()
  
  rmse <- vector()
  mtry <- vector()
  
  hyper_grid <- build_hyperparameter_dataframe(
    mtry_frac = mtry_frac,
    min.node.size = min.node.size,
    sample.fraction = sample.fraction,
    ntrees = ntrees,
    PEMs = PEMs,
    weight_list = weight,
    phylo = phylo)
  
  optimized_parameter <- pbmcapply::pbmclapply(1:nrow(hyper_grid), function(i) {
    
    if(phylo == TRUE){
      
      names1 <- colnames(data)[colnames(data) %in% colnames(trait_data)]
      names2 <- paste("eig", 1:hyper_grid$PEMs[i], sep = "")
      data_new <- data[c(names1, names2)]
      
    }
    
    if(phylo == FALSE){
      
      data_new <- trait_data
      
    }
    
    n_features_new <- dim(data_new)[2] - 2
    
    fit <- ranger::ranger(formula = formula,
                          data = data_new,
                          num.trees = hyper_grid$ntrees[i],
                          mtry = round(hyper_grid$mtry_frac[i]*n_features_new),
                          min.node.size = hyper_grid$min.node.size[i],
                          replace = hyper_grid$replace[i],
                          sample.fraction = hyper_grid$sample.fraction[i],
                          verbose = FALSE,
                          seed = 123,
                          respect.unordered.factors = 'order',
                          case.weights = weight[[hyper_grid$wgt[i]]])
    
    # export OOB error
    rmse[i] <- fit$prediction.error
    mtry[i] <- round(hyper_grid$mtry_frac[i]*n_features)
    
    final_objects <- data.frame(default_rmse = default_rmse,
                                rmse = rmse[i],
                                mtry = mtry[i])
    
    return(final_objects)
    
  }, mc.cores = cores)
  
  optimized_parameter_bind <- do.call(rbind, optimized_parameter)
  
  merged_optimized_parameter <- cbind(optimized_parameter_bind, hyper_grid)
  
  return(merged_optimized_parameter)
  
}

