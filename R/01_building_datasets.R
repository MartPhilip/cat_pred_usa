building_dataset <- function(test){
  #loading IUCN info for bird species
  ##selecting the right country, Italy if test is NULL or United States if not
  if(test==FALSE){country <- 'Italy'}
  else{country <- 'United States'}
  path_raw_data_iucn_countries <- here::here('data','raw-data','IUCN','countries.csv')
  iucn_raw_countries <- utils::read.csv(path_raw_data_iucn_countries)
  iucn_countries <- base::subset(iucn_raw_countries, select = c('scientificName','name','presence','origin'))
  iucn_countries$scientificName <- gsub(' ','_',iucn_countries$scientificName)
  iucn_countries <- iucn_countries[iucn_countries$name ==country,]
  iucn_countries <- base::subset(iucn_countries,select=-name)
  iucn_countries <-  dplyr::distinct(iucn_countries,scientificName,.keep_all = T)
  ##selecting the birds only
  path_raw_data_iucn_summaries <- here::here('data','raw-data','IUCN','simple_summary.csv')
  iucn_raw_summaries <- utils::read.csv(path_raw_data_iucn_summaries)
  iucn_summaries <- base::subset(iucn_raw_summaries, select = c('scientificName','className'))
  iucn_summaries$scientificName <- base::gsub(' ','_',iucn_summaries$scientificName)
  iucn_summaries <- iucn_summaries[iucn_summaries$className =='AVES',]
  iucn_summaries <-  dplyr::distinct(iucn_summaries,scientificName,.keep_all = T)
  
  iucn <- base::merge(iucn_summaries,iucn_countries,by='scientificName',all.x = T)
  iucn <- base::subset(iucn,select=-className)
  iucn <- stats::na.omit(iucn)
  
  #loading traits info for bird species
  path_raw_data_trait <- here::here('data','raw-data','Traits','bird_traits_from_Marino_and_Bellard_2023')
  bird_traits_all<- base::readRDS(path_raw_data_trait)
  bird_traits_all$binomial <- base::gsub(' ','_',bird_traits_all$binomial)
  path_raw_data_insularity <- here::here('data','raw-data','Traits','bird_insularity_from_Marino_et_al_2021')
  bird_insularity_all<-base::readRDS(path_raw_data_insularity)
  bird_insularity_all <- base::subset(bird_insularity_all, select = c('binomial','insular_endemic'))
  
  bird_insularity_all$binomial <- base::gsub(' ','_',bird_insularity_all$binomial)
  bird_traits_all<-base::merge(bird_traits_all,bird_insularity_all,by='binomial',all.x=T)
  
  
  #Merging IUCN info + traits of bird species
  bird_data <- merge(iucn,bird_traits_all,by.y='binomial',by.x='scientificName',all.x=T) 
  
  #loading habitat info for bird species
  habitat_iucn_data_modif <- extracting_habitat_info(country)
  habitat_iucn_data_modif$scientificName <- base::gsub(' ','_',habitat_iucn_data_modif$scientificName)
  
  
  #Merging IUCN info + traits + habitats of bird species
  bird_data <- merge(bird_data,habitat_iucn_data_modif,by='scientificName',all.x=T) 
  
  
  #Adpating trait values
  bird_data <- dplyr::mutate(bird_data, Mass=exp(ln.Mass)) 
  bird_data <-  dplyr::mutate(bird_data, Clutch.size=exp(ln.Clutch)) 
  bird_data <-  dplyr::mutate(bird_data, Beak.depth=exp(ln.Beak.Depth)) 
  bird_data <-  dplyr::mutate(bird_data, Beak.length.nares=exp(ln.Beak.Length_Nares))
  bird_data$Trophic.Niche <- as.factor(bird_data$Trophic.Niche)
  bird_data$Primary.Lifestyle <- as.factor(bird_data$Primary.Lifestyle)
  
  
  #loading distribution info for bird species
  if(test==FALSE){
  path_bird_distribution_data <- here::here('data','raw-data','Distribution','range_birds_Italy.RDS')
  bird_distribution_data <- base::readRDS(path_bird_distribution_data)
  }
  else{
    path_bird_distribution_data <- here::here('data','raw-data','Distribution','range_birds_USA.RDS')
    bird_distribution_data <- base::readRDS(path_bird_distribution_data)
  }
  bird_distribution_data <- dplyr::distinct(bird_distribution_data,binomial,.keep_all = T)
  bird_distribution_data <- dplyr::rename(bird_distribution_data,Geographical.range=home_range)
  
  
  bird_data <- base::merge(bird_data,bird_distribution_data,by.x='scientificName',by.y='binomial',all.x = T)
  
  
  
  #Removing species which do not match my criteria
  bird_data <- bird_data[bird_data$presence =='Extant',]
  bird_data <- bird_data[bird_data$origin =='Native',]
  bird_data$insular_endemic[is.na(bird_data$insular_endemic)] <- 1
  bird_data <- bird_data[bird_data$insular_endemic ==0,]
  bird_data <- bird_data[bird_data$Geographical.range>0,]
  
  
  #Removing column with same info or not utilise after
  bird_data <- dplyr::select(bird_data, -c('presence','origin','volant',
                                                   'ln.Mass','ln.Clutch','ln.Beak.Depth','ln.Beak.Length_Nares',
                                                   'Beak.depth','insular_endemic','Habitat','Beak.length.nares'))
  if(test ==FALSE){
  path_response_data <- here::here('data','raw-data','Mori-et-al','Italian_prey_brought_home.xlsx')
  response_data <- readxl::read_xlsx(path_response_data)
  response_data$binomial <- base::gsub(' ','_',response_data$binomial)
  response_data <- response_data[response_data$taxon=='Birds',]
  response_data <- base::subset(response_data,select=-taxon)
  response_data <- base::subset(response_data,select=-order)
  bird_data <- merge(bird_data,response_data,by.x='scientificName',by.y = 'binomial',all.x = T)
  bird_data$response[is.na(bird_data$response)] <- 0
  }
  return(bird_data)
}


bird <- building_dataset(test=TRUE)
