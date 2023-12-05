conservation_status_of_prey <- function(ACAD_data,PIF){
  if(PIF=='PS.g'){name<-'Population size'}
  if(PIF=='BD.g'){name<-'Breeding distribution'}
  if(PIF=='TB.c'){name<-'Threats during breeding seasons'}
  if(PIF=='PT.c'){name<-'Population trend'}
  ACAD<- ACAD_data |> dplyr::filter(name==PIF)
  anova <- aov(sqrt(prediction) ~ value , data = ACAD)
  tukey<- TukeyHSD(anova)
  tukey <- tukey[[1]]
  tukey<- as.data.frame(tukey)
  tukey$groups <- rownames(tukey)
  colnames(tukey) <- c("diff","lwr","upr","p_adj","groups")
  plot <- ggplot(tukey, aes(x = groups, y = diff)) +
    geom_bar(stat = "identity", aes(fill = factor(ifelse(p_adj < 0.05, "Significant", "Non-significant"))), width = 0.7) +
    geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2, color = "black") +
    ggrepel::geom_label_repel(aes(label=round(p_adj,3)),fill='black',color='white',size=2.5) +
    labs(x = "", y = "Difference", fill = "Significance",title=name) +
    theme_classic() +
    scale_fill_viridis_d()+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1),legend.position = 'none')+
    geom_hline(yintercept = 0, linetype='dashed',color='black')
  return(plot)
}
