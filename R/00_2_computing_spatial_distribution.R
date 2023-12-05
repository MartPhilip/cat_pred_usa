#computing_spatial_distribution <- function(country,species_data){
  # polygons form countries
  #poly_countr <- rnaturalearth::ne_countries(returnclass = "sf", type = "map_units")
  #poly_countr  <- dplyr::select(poly_countr, name, geometry)
  #yepoly_countr <- sf::st_make_valid(poly_countr)
  #country_geom <- dplyr::filter(yepoly_countr,name== country)
 #path_birdlife_distribution <- here::here('data','raw-data','Distribution','bird_nat_range.RDS')
 #bird_nat_range <- base::readRDS(path_birdlife_distribution)
 #bird_nat_range$binomial <- base::gsub(" ","_",bird_nat_range$binomial)
 #bird_nat_range_all_species <- dplyr::filter(bird_nat_range, binomial %in% species_data)
 #country_geom <- sf::st_transform(country_geom, 4326)
 #country_geom <- sf::st_make_valid(country_geom)
  
#n_cells <- function(poly, rast){
    # from a raster containing 1 for each cell in continent
    # keep cells that are masked by the polygon = cells from native range 
#masked_rast <- raster::mask(rast, poly)
    
    # calcute number of cells with a value = cells in contin & in poly
# data <- raster::as.data.frame(masked_rast, xy = TRUE) 
#data <- dplyr::filter(data,!is.na(layer))# remove empty cells
#range <- nrow(data)
    
# return(range)
#  }
  
  # initialize raster
#  if(country == 'United States'){
#  extent <- raster::extent(-125, -66, 24, 49)}
#  else{extent <- raster::extent(6, 19, 36, 47)}
  # Create a raster with a resolution of 1 km x 1 km
#  raster_resolution <- raster::raster(xmn = extent@xmin, xmx = extent@xmax, 
# ymn = extent@ymin, ymx = extent@ymax, 
#crs = "+proj=longlat +datum=WGS84 +no_defs", 
#res=c(0.25,0.25))
  
  # Plot the raster
#raster_resolution[] <- 1
  
  # initialise european raster
#raster_resolution_country <- raster::mask(raster_resolution, country_geom)
# area <- raster_resolution_country@data@values
#  area <- na.omit(area)
#  sum(area)
  
#  for(i in 1:nrow(bird_nat_range_all_species)){
    #select poly to filter
#    sp_poly <- bird_nat_range_all_species[i,]
    #save species name
#    bird_nat_range_all_species$binomial[i] = bird_nat_range_all_species$binomial[i]
    #calculate and save cell number
#    bird_nat_range_all_species$range_n_cells[i] = n_cells(sp_poly, raster_resolution_country)
    
    #save output
#    print(i)
# }
  
  #  range_bird_nat_range_all_species <- plyr::ddply(bird_nat_range_all_species,.(binomial),summarize,home_range=sum(range_n_cells))
  #  return(range_bird_nat_range_all_species)
#}
