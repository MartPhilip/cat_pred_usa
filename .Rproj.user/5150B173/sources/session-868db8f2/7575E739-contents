devtools::load_all()
#Compiling training dataset i,e, birds from Italy
training_dataset <- building_dataset(test=FALSE)


#Extracting phylogenetic info
path_eigenvector_dataset <- here::here('data','raw-data','Phylogeny','bird_eig400.rds')
path_ID_birds_phylogeny <- here::here('data','raw-data','Phylogeny','birdsID.rds')
ev400 <- base::readRDS(path_eigenvector_dataset)
IDn <- base::readRDS(path_ID_birds_phylogeny)


#fix the rownames and get rid of "_"
IDn <- IDn[[2]]
ev400$ID <- rownames(ev400)
IDn$ID <- gsub(":",".", IDn$ID)
ev400 <- merge(ev400, IDn, by="ID", all.x=T)
training_dataset <- training_dataset[,!(names(training_dataset)%in%c("X"))]
names(training_dataset)[names(training_dataset)=="scientificName"] <- "targetTaxonName"


#add phylo eigenvectors
ev400$ID <- NULL
names(ev400)[names(ev400)=="sp"] <- "scientificName"
ev400$scientificName <- base::gsub(" ","_",ev400$scientificName )
training_dataset <- merge(training_dataset, ev400, by.x ="targetTaxonName" ,by.y ="scientificName", all.x=TRUE)
training_dataset <- training_dataset[complete.cases(training_dataset),] #all complete
training_dataset$response <- training_dataset$response/145


#preparing weight list 
wgts <- training_dataset$response
wgts.1 <- rep(1,length(wgts))
ints <- length(wgts[wgts>0])
nons <- length(wgts[wgts==0])
wgts <- ifelse(wgts>0, 1, ints/nons)
wgts.2 <- wgts
for(i in 1:length(wgts)){
  if (training_dataset$response[i]>0.03){
    wgts[i] <- (ints/nons)*3
  }
  else if (training_dataset$response[i]>0 & training_dataset$response[i]<0.03){
    wgts[i] <- (ints/nons)*2
  }
  else{}
}
wgts.3 <- wgts
wgtlist <- list(wgts.1,wgts.2,wgts.3)


#Preparing data for the random forest
trait_data <- training_dataset[,c(1:15)]

trait_data <- janitor::clean_names(trait_data)
training_dataset <- janitor::clean_names(training_dataset)


#Computing the random forest
#RF.opti <- computing_random_forest(data=training_dataset,
                                   #species='target_taxon_name',
                                   #response_data='response',
                                   #trait_data=trait_data,
                                   #phylo_data = TRUE,
                                   #classification = FALSE, # binary or regression
                                   #weight = wgtlist,
                                   #mtry_frac = c(.05, .15, .25, .333, .4, .6),
                                   #min.node.size = c(1, 3, 5, 10, 20, 30, 50),
                                   #sample.fraction = c(.5, .6, .7),
                                   #ntrees = seq(50,750,50),
                                   #PEMs = c(5,10,20,50),
                                   #cores = 4)

path_RF.opti <- here::here('data','derived-data','RF.opti.RDS')
RF.opti <- base::readRDS(path_RF.opti)

#Evaluating the random forest

RF.quali <- random_forest_quality(data=training_dataset,
                                  species='target_taxon_name',
                                  response_data='response',
                                  trait_data=trait_data,
                                  phylo_data = TRUE,
                                  classification = FALSE, # binary or regression
                                  weight = wgtlist,
                                  RF.opti, #results of the optimized_RF_function()
                                  until_which_model=100)

RF.quali.metrics <- RF.quali[[1]]
RF.quali.models <- RF.quali[[2]]

RF.quali.metrics <- base::do.call(rbind,RF.quali.metrics)

#Compiling testing dataset i,e, birds from the United States
testing_dataset <- building_dataset(test=TRUE)
testing_dataset <- testing_dataset[,!(names(testing_dataset)%in%c("X"))]
names(testing_dataset)[names(testing_dataset)=="scientificName"] <- "targetTaxonName"
testing_dataset <- merge(testing_dataset, ev400, by.x ="targetTaxonName" ,by.y ="scientificName", all.x=TRUE)
testing_dataset <- testing_dataset[complete.cases(testing_dataset),] #all complete
testing_dataset <- testing_dataset[,c(1:24)]
testing_dataset <- janitor::clean_names(testing_dataset)

#fitting best RF
training_data_opti <- training_dataset[,c(1:25)]
fit <- ranger::ranger(formula = response~.-target_taxon_name,
                      data = training_data_opti,
                      num.trees = 50,
                      mtry = 4,
                      min.node.size = 30,
                      replace = FALSE,
                      sample.fraction = 0.7,
                      verbose = FALSE,
                      seed = 123,
                      respect.unordered.factors = 'order',
                      case.weights = wgtlist[[3]])
training_prediction <- predict(fit,training_data_opti)
Ypred <- training_prediction[["predictions"]]
Yobs<- training_data_opti$response
see_prediction <- data.frame(Ypred=Ypred,Yobs=Yobs)
plot(see_prediction$Yobs,see_prediction$Ypred)
training_data_opti$prediction <- Ypred


testing_prediction <- predict(fit,testing_dataset)
testing_dataset$prediction <- testing_prediction[["predictions"]]


#SHAP
pfun <- function(object, newdata) {  # prediction wrapper
  unname(predict(object, data = newdata)$predictions)
}
set.seed(123)  # for reproducibility
X <- training_data_opti |> dplyr::select(-target_taxon_name,-response)
ex.t1 <- fastshap::explain(fit, X = X, pred_wrapper = pfun, nsim = 100, adjust = TRUE,
                           shap_only = FALSE)
tibble::as_tibble(ex.t1$shapley_values)
shv.global <- shapviz::shapviz(ex.t1)
importance_Italy <- shapviz::sv_importance(shv.global, show_numbers = TRUE) 
importance_Italy <- importance_Italy + theme_classic()+ labs(title='C')

#Plot training
Figure1 <- plot_training_dataset(training_data_opti,importance_Italy)
Figure1


#grouping species
training_data_opti_predonly <- training_data_opti[training_data_opti$prediction>=0.0068,]
predation_pressure_groups <- quantile(training_data_opti_predonly$prediction)
testing_dataset <- testing_dataset |> dplyr::mutate('Predation pressure'=dplyr::case_when(prediction<predation_pressure_groups[1]~'No predation',
                                                                                          prediction>=predation_pressure_groups[1] & prediction<predation_pressure_groups[2]~'Low predation',
                                                                                          prediction>=predation_pressure_groups[2] & prediction<predation_pressure_groups[3]~'Medium predation',
                                                                                          prediction>=predation_pressure_groups[3] & prediction<predation_pressure_groups[4]~'High predation',
                                                                                          prediction>=predation_pressure_groups[4]~'Very high predation',))
testing_dataset$`Predation pressure` <- factor(testing_dataset$`Predation pressure`,levels=c('No predation','Low predation','Medium predation','High predation','Very high predation'))


#Quantifying predation pressure
training_predation_pressure_summary <- predation_pressure_info(Yobs)
training_prediction_predation_pressure_summary <- predation_pressure_info(Ypred)
testing_prediction_predation_pressure_summary <- predation_pressure_info(testing_dataset$prediction)





#Trait analyse

##Checking association between features
trait_testing_dataset <- testing_dataset |> dplyr::select(-prediction,-`Predation pressure`,
                                                          -forest,-shrubland,-wetlands,-grassland,
                                                          -artificial_terrestrial,-rocky,-target_taxon_name)
corr_trait <- corr_mixed_assoc(trait_testing_dataset)
corr_trait <- corr_trait |> dplyr::rename(Predictor1=x) |> dplyr::rename(Predictor2=y)
corr_trait$assoc <- abs(corr_trait$assoc)
plot_corr_trait<- ggplot(data=corr_trait,aes(x = Predictor1,y=Predictor2,label=round(assoc,2),color=assoc,size=assoc))+
  geom_text(color='black')+
  scale_x_discrete(guide=guide_axis(n.dodge=3)) +
  theme_bw()+
  theme(legend.position = 'none')+
  labs(x='',y='')
plot_corr_trait

##Functional space
pca_data <- functional_space_building(testing_dataset,c('mass','hand_wing_index','clutch_size'),2)
pcload <- pca_data[["sqload"]]
pca_data$eig
round(pca_data$sqload,digit=2)
par(mfrow=c(2,2))
plot(pca_data,choice="ind",label=FALSE,axes=c(1,2),
     main="Observations before rotation")
plot(pca_data,choice="ind",label=FALSE,axes=c(1,2),
     main="Observations after rotation")
plot(pca_data,choice="cor",main="Numerical variables")
plot(pca_data,choice="cor",main="Numerical variables")
par(mfrow=c(1,1))

pca_coord <- pca_data[["ind"]][["coord"]]
pca_coord <- as.data.frame(pca_coord)
pca_coord$target_taxon_name <- testing_dataset$target_taxon_name
info_for_pca <- subset(testing_dataset,select=c('target_taxon_name','Predation pressure','prediction'))
pca_data_for_plot <- merge(pca_coord,info_for_pca,by='target_taxon_name',all.x = T)
pca_data_for_plot <- pca_data_for_plot |> dplyr::rename('Random Forest predation prediction'=prediction) |>
  dplyr::rename('Threshold predation prediction'=`Predation pressure`)|>
  dplyr::rename('First principal component (45%)'=dim1.rot)|>
  dplyr::rename('Second principal component (39%)'=dim2.rot)

pcload <- as.data.frame(pcload)
pcload$trait <- rownames(pcload)
pcload$dim1.rot <- pcload$dim1.rot*5
pcload$dim2.rot <- pcload$dim2.rot*5
threshold <- data.frame('Threshold predation prediction'=c('Max. no predation','Max. low predation','Max. medium predation','Max. high predation','Max. very high predation'),
                        'Random Forest predation prediction' = c(predation_pressure_groups[1],predation_pressure_groups[2],predation_pressure_groups[3],predation_pressure_groups[4],max(testing_dataset$prediction)))

Figure2_1 <- plot_test_dataset(testing_dataset,threshold)
Figure2_2 <- plot_functional_space(pca_data_for_plot,pcload)
Figure2 <- Figure2_1/Figure2_2
Figure2

number_of_species_per_group <- pca_data_for_plot |>
  dplyr::mutate(occurence=1) |>
  dplyr::group_by(`Threshold predation prediction`) |>
  dplyr::summarise(number=sum(occurence))

#Habitat analyses
Figure3 <- comparing_habitat(testing_dataset)
Figure3

#Conservation analyses
path_ACAD_database <- here::here('data','raw-data','ACAD','ACAD.csv')
ACAD <- utils::read.csv(path_ACAD_database)
ACAD <- ACAD |> dplyr::select(Scientific.Name,PS.g,BD.g,TB.c,PT.c,)
ACAD$Scientific.Name <- base::gsub(" ","_",ACAD$Scientific.Name)

ACAD_data <- merge(testing_dataset,ACAD,by.x="target_taxon_name",by.y="Scientific.Name",all.x=T)
ACAD_data <- ACAD_data |> dplyr::select(prediction,PS.g,BD.g,TB.c,PT.c) |> tidyr::pivot_longer(cols=c("PS.g","BD.g","TB.c","PT.c"))
ACAD_data$value <- as.factor(ACAD_data$value)

PIF.factor <- c("PS.g","BD.g","TB.c","PT.c")
Figure4.1 <- conservation_status_of_prey(ACAD_data, PIF = PIF.factor[1])
Figure4.2 <- conservation_status_of_prey(ACAD_data, PIF = PIF.factor[2])
Figure4.3 <- conservation_status_of_prey(ACAD_data, PIF = PIF.factor[3])
Figure4.4 <- conservation_status_of_prey(ACAD_data, PIF = PIF.factor[4])
Figure4 <- Figure4.1/Figure4.2/Figure4.3/Figure4.4
Figure4

#Comparison with empirical records
path_lepczyk_et_al_data <- here::here('data','raw-data','Lepczyk-et-al','bird_database.csv')
data_lepczyk <- utils::read.csv(path_lepczyk_et_al_data)
data_lepczyk <- subset(data_lepczyk,select=-X)

testing_dataset <- base::merge(testing_dataset,data_lepczyk,by.x='target_taxon_name',
                               by.y = 'binomial',all.x = T)
testing_dataset <- testing_dataset |> dplyr::mutate('Species type'=dplyr::case_when(
  Prey == 1 ~ 'Empirical record',
  Prey == 0 ~ 'No record'
))

data_pred_vs_empirical <- subset(testing_dataset,select=c('prediction','Species type'))
data_pred_vs_empirical_sum <- data_pred_vs_empirical |> dplyr::group_by(`Species type`) |>
  dplyr::summarise(median=median(prediction))

p_value_permutation_test <- permutation_test(data=data_pred_vs_empirical,num_permutations = 1000)
p_value_permutation_test

Figure5.1 <- plot_permutation(data_pred_vs_empirical)
Figure5.2 <- plot_distribution_record(testing_dataset)
Figure5 <- Figure5.1/Figure5.2
Figure5





