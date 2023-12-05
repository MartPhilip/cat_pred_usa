plot_distribution_record <- function(data){
  data_sum <- data |> dplyr::mutate(occurence = 1) |>  dplyr::group_by(`Species type`,`Predation pressure`)  |>  dplyr::summarise('Number of species'=sum(occurence))
  plot <- ggplot(data = data_sum, aes(x = `Predation pressure` , y = `Number of species` , fill = `Predation pressure`,label = `Number of species`))+
  facet_grid(~`Species type`)+
  geom_col()+
  geom_text(vjust=-0.2)+
  scale_y_sqrt() +
  scale_fill_viridis_d()+
  scale_x_discrete(guide=guide_axis(n.dodge=2)) +
  theme_classic()+
  theme(legend.position = 'none')+
  labs(x='',y='Number of species',title='B')
  return(plot)
}

