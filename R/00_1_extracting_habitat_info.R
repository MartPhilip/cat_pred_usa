extracting_habitat_info <- function(country){
  if(country=='Italy'){path_raw_data_iucn_habitat <- here::here('data','raw-data','IUCN','habitats_Italy.csv')}
  else{path_raw_data_iucn_habitat <- here::here('data','raw-data','IUCN','habitats_USA.csv')}
  habitat_iucn_data <- utils::read.csv(path_raw_data_iucn_habitat)
  habitat_iucn_data <- habitat_iucn_data[habitat_iucn_data$suitability =="Suitable",]
  habitat_iucn_data$habitat <- stringr::str_split(habitat_iucn_data$name, " - ", simplify = TRUE)[,1]
  habitat_iucn_data <- base::subset(habitat_iucn_data, select = c("scientificName","habitat"))
  habitat_iucn_data$habitat <- stringr::str_extract(habitat_iucn_data$habitat, "\\S+")
  habitat_iucn_data <- dplyr::distinct(habitat_iucn_data,scientificName,habitat)
  habitat_iucn_data$value <- 1
  habitat_iucn_data <- tidyr::pivot_wider(habitat_iucn_data,names_from = habitat, values_from = value, values_fill = 0)
  habitat_iucn_data <- base::subset(habitat_iucn_data, select =c("scientificName","Shrubland"
  ,"Grassland","Wetlands","Artificial/Terrestrial","Rocky","Forest"))
  return(habitat_iucn_data)
}



