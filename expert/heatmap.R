drawheatmap <-
  function(data,
           Label,
           strTitle = "Heatmap",
           cluster_row = T,
           cluster_col = F) {
    library(pheatmap)
    df11 <- as.data.frame(data)
    df11[is.na(df11)] <- 0
    ann_col <- data.frame(type = Label, row.names = names(df11))
    ann_col$type <- factor(ann_col$type)
    # strain_color <- brewer.pal(10,"Set3")[5:6]
    # names(strain_color) <- c("C","R")
    # ann_colors <- list(stain = strain_color)
    
    pheatmap(
      df11,
      color = brewer.pal(11, "RdYlBu")[11:1],
      fontsize_col = 8,
      annotation_col = ann_col,
      #annotation_colors = ann_colors,
      cluster_rows = cluster_row,
      cluster_cols = cluster_col,
      show_rownames = F,
      show_colnames = T,
      main = strTitle
    )
  }
