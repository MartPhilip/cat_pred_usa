plot_permutation <- function(data){
# Calculate median values for each Species type
 median_values <- aggregate(prediction ~ `Species type`, data = data, FUN = median)
 significant <- TRUE
 colors <- viridis::viridis_pal(option = "D")(2)
 # Plotting
 permu_plot <- ggplot(data, aes(x = prediction, fill = `Species type`)) +
  geom_density(alpha = 0.4) +  # Density plots for each group
  geom_point(data = median_values, aes(x = prediction, y = prediction), 
             color = "black", shape = 5, size = 3) +  # Median points
  labs(x = "Prediction", y = "Density", fill = "Species type") +
  scale_fill_manual(values = colors) +  # Set color palette
  theme_classic() +
  labs(y='Density',x='Random Forest predation prediction',title='A')+
  scale_x_sqrt() +
  theme(legend.position = c(0.8, 0.8)) +
  geom_text(data = median_values, aes(x = prediction, y = 0.005, label = "*"), 
            size = 6, vjust = -0.5, hjust = 0.5, color = ifelse(significant, "black", "gray"))+
  geom_vline(xintercept = 0.006896552,linetype='dashed')
 return(permu_plot)
}

