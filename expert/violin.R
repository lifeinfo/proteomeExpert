drawviolin <-
  function(value,
           sample,
           strTitle = "Violin plot",
           cluster_row = T,
           cluster_col = F) {
    df <- data.frame(sample = sample , value = value)
    ggplot(df, aes(x = sample, y = value, fill = sample)) +
      geom_violin(trim = FALSE) +
      labs(ylab = "Value", xlab = "Violin") +
      geom_boxplot(width = 0.1) +
      theme(
        legend.direction = 'horizontal',
        legend.position = 'top',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")
      )
    
  }
