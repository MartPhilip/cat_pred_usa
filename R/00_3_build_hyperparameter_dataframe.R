build_hyperparameter_dataframe <- function(mtry_frac=NULL,
                                           min.node.size=NULL,
                                           sample.fraction=NULL,
                                           ntrees=NULL,
                                           weight_list=weight_list,
                                           PEMs=NULL,
                                           phylo){
  if(phylo == FALSE){
    if(is.null(mtry_frac)){mtry_frac <- c(.05, .15, .25, .333, .4, .6)}
    if(is.null(min.node.size)){min.node.size <- c(1, 3, 5, 10, 20, 30, 50, 75, 100)}
    if(is.null(sample.fraction)){sample.fraction = c(.5, .6, .7)}
    if(is.null(ntrees)){ntrees <-seq(50,750,50)}
    wgt <- c(1:length(weight_list))
    replace <- c(TRUE, FALSE)
    hyper_grid <- base::expand.grid( mtry_frac = mtry_frac,
                                     min.node.size = min.node.size,
                                     replace = replace,
                                     sample.fraction = sample.fraction,
                                     ntrees = ntrees,
                                     wgt = wgt)
    return(hyper_grid)}
  if(phylo == TRUE){
    if(is.null(mtry_frac)){mtry_frac <- c(.05, .15, .25, .333, .4, .6)}
    if(is.null(min.node.size)){min.node.size <- c(1, 3, 5, 10, 20, 30, 50, 75, 100)}
    if(is.null(sample.fraction)){sample.fraction = c(.5, .6, .7)}
    if(is.null(ntrees)){ntrees <-seq(50,750,50)}
    if(is.null(PEMs)){PEMs <-c(1,5,10)}
    wgt <- c(1:length(weight_list))
    replace <- c(TRUE, FALSE)
    hyper_grid <- base::expand.grid( mtry_frac = mtry_frac,
                                     min.node.size = min.node.size,
                                     replace = replace,
                                     sample.fraction = sample.fraction,
                                     ntrees = ntrees,
                                     wgt = wgt,
                                     PEMs=PEMs)
    return(hyper_grid)}
}

