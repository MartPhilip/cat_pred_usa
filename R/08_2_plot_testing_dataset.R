plot_test_dataset <- function(testing_dataset,threshold){
rac_pred<- ggplot(data=testing_dataset,aes(x=reorder(target_taxon_name,-prediction),y=prediction,fill=prediction,color=prediction))+
  geom_col()+
  theme_classic()+
  labs(x='species',y='Random Forest predation prediction',title='A')+
  theme(legend.position = 'none', axis.text.x = element_blank(),axis.ticks.x = element_blank())+
  scale_fill_viridis_c(option='viridis',direction = 1)+
  scale_color_viridis_c(option='viridis',direction = 1)+
  ylim(0,0.17)
threshold$Threshold.predation.prediction <- factor(threshold$Threshold.predation.prediction,levels=c('Max. no predation','Max. low predation','Max. medium predation','Max. high predation','Max. very high predation'))
threshold<- ggplot(data = threshold,aes(y=Random.Forest.predation.prediction,x=reorder(Threshold.predation.prediction,-Random.Forest.predation.prediction),fill=Threshold.predation.prediction,color=Threshold.predation.prediction,label=round(Random.Forest.predation.prediction,4)))+
  geom_point(shape=23,size=10)+
  geom_text(color='black',vjust = -2)+
  scale_fill_viridis_d(option='viridis')+
  scale_color_viridis_d(option='viridis')+
  labs(x='',y='',title = "B")+
  theme_classic()+
  theme(legend.position='none')+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+
  ylim(0,0.17)
  plot <- rac_pred|threshold
}
