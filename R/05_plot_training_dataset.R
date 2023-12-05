plot_training_dataset <- function(trainging_data,importance_plot){
RAC_obs<- ggplot(data=trainging_data,aes(x=reorder(target_taxon_name,-response),y=response,fill=response,color=response))+
  geom_col()+
  theme_classic()+
  labs(x='species',y='Predation events per year per cat',title='A')+
  theme(legend.position = 'none', axis.text.x = element_blank(),axis.ticks.x = element_blank())+
  scale_fill_viridis_c(option='viridis',direction = 1)+
  scale_color_viridis_c(option='viridis',direction = 1)+
  ylim(0,0.65)
RAC_pred <- ggplot(data=trainging_data,aes(x=reorder(target_taxon_name,-prediction),y=prediction,fill=prediction,color=prediction))+
  geom_col()+
  theme_classic()+
  labs(x='species',y='Predation prediction',title='B')+
  theme(legend.position = 'none', axis.text.x = element_blank(),axis.ticks.x = element_blank())+
  scale_fill_viridis_c(option='viridis',direction = 1)+
  scale_color_viridis_c(option='viridis',direction = 1)+
  ylim(0,0.25)
plot <- (RAC_obs)|(RAC_pred/importance_plot)
return(plot)
}
