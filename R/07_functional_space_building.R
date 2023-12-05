functional_space_building <- function(data,trait_vector,dim){
  data_for_pca <- subset(data,select=trait_vector)
  data_for_pca <- data_for_pca |> dplyr::mutate(log_mass=log(mass)) |>
    dplyr::mutate(log_clutch_size=log(clutch_size))|>
    dplyr::select(-mass,-clutch_size)
  split <- PCAmixdata::splitmix(data_for_pca)
  X1 <- split$X.quanti 
  X2 <- split$X.quali 
  res.pcamix <- PCAmixdata::PCAmix(X.quanti=X1, X.quali=X2,rename.level=TRUE,
                       graph=FALSE)
  res.pcarot <- PCAmixdata::PCArot(res.pcamix,dim=dim,graph=FALSE)
}
