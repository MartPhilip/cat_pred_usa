comparing_habitat <- function(testing_dataset){
  habitat_data <- testing_dataset |> dplyr::select(target_taxon_name,wetlands,shrubland,
                                                grassland,wetlands,artificial_terrestrial,rocky,forest,prediction)
  habitat_data <- habitat_data |> tidyr::pivot_longer(cols=c('wetlands','shrubland','grassland','wetlands',
                                                              'artificial_terrestrial','rocky','forest'))
  habitat_data <- habitat_data |> dplyr::filter(value==1) |> dplyr::select(-value)
  anova_result <- aov(sqrt(prediction) ~ name , data = habitat_data)
  tukey_result <- TukeyHSD(anova_result)
  tukey_result <- tukey_result[[1]]
  tukey_result <- as.data.frame(tukey_result)
  tukey_result$groups <- rownames(tukey_result)
  colnames(tukey_result) <- c("diff","lwr","upr","p_adj","groups")
  get_asterisks <- function(p_value) {
    if (p_value < 0.001) return("***")
    else if (p_value < 0.01) return("**")
    else if (p_value < 0.05) return("*")
    else if (p_value < 0.1) return("°")
    else return("")
  }
  
  # Create a new column with asterisks based on p-value
  tukey_result$asterisks <- sapply(tukey_result$p_adj, get_asterisks)
  # Create ggplot with letters indicating significant differences
  habitat_plot <- ggplot(tukey_result, aes(x = groups, y = diff)) +
    geom_bar(stat = "identity", aes(fill = factor(ifelse(p_adj < 0.05, "Significant", "Non-significant"))), width = 0.7) +
    geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2, color = "black") +
    ggrepel::geom_label_repel(aes(label=round(p_adj,3)),fill='black',color='white') +
    labs(x = "", y = "Difference", fill = "Significance") +
    theme_classic() +
    scale_fill_viridis_d()+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
    geom_hline(yintercept = 0, linetype='dashed',color='black')+
    theme(legend.position = c(0.8, 0.8))
  return(habitat_plot)
}
