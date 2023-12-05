plot_functional_space <- function(pca_data_for_plot,pcload){
  pca_plot <- ggplot2::ggplot(pca_data_for_plot, ggplot2::aes(x = `First principal component (45%)`, y = `Second principal component (39%)`)) +
  ggplot2::geom_point(data = pca_data_for_plot, ggplot2::aes(color=`Threshold predation prediction`,size=`Threshold predation prediction`),alpha=0.7) +
  theme_bw()+
  theme(legend.position = 'bottom',legend.box = "vertical",legend.title = element_blank(),legend.text = element_text(size = 7),axis.text.x = element_text(size = 7),axis.text.y = element_text(size = 7))+
  ggplot2::geom_segment(data = pcload, ggplot2::aes(x = 0, y = 0, xend = dim1.rot, yend = dim2.rot), arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")), colour = "black") +
  ggplot2::geom_text(data = pcload, ggplot2::aes(x = dim1.rot, y = dim2.rot, label = trait), size = 3, nudge_x = c(0, 0, 0, 0, -0.6), nudge_y = c(0.3, -0.2, -0.4, 0.3, -0.3))+
  scale_color_viridis_d(option='viridis')+
  labs(title='C')
  PC1 <- ggplot(data = pca_data_for_plot,aes(x = `First principal component (45%)`,group=`Threshold predation prediction`,fill=`Threshold predation prediction`))+geom_density(color='black',adjust=1.5, position="fill",alpha=0.7) + theme_classic() + theme(legend.position = 'none')+scale_fill_viridis_d(option='viridis')+labs(title = 'D')
  PC2 <- ggplot(data = pca_data_for_plot,aes(x = `Second principal component (39%)`,group=`Threshold predation prediction`,fill=`Threshold predation prediction`))+geom_density(color='black',adjust=1.5, position="fill",alpha=0.7) + theme_classic() + theme(legend.position = 'none')+scale_fill_viridis_d(option='viridis')+labs(title = 'E')
  plot_funcio <- pca_plot | (PC1 / PC2)
return(plot_funcio)
}

