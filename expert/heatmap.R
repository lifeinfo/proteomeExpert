drawheatmap <-
  function(data,
           Label,
           strTitle = "Heatmap",
           cluster_row = T,
           cluster_col = F) {
    df11 <- as.data.frame(data)
    df11[is.na(df11)] <- 0
    ann_col <- data.frame(type = Label, group = Label, row.names = row.names(df11))
    ann_col$type <- factor(ann_col$type)
    # strain_color <- brewer.pal(10,"Set3")[5:6]
    # names(strain_color) <- c("C","R")
    # ann_colors <- list(stain = strain_color)

    p <- heatmaply(
      df11,
      #colors = brewer.pal(11, "RdYlBu")[11:1],
      column_text_angle = 45,
      fontsize_col = 8,
      #annotation_col = ann_col,
      #annotation_colors = ann_colors,
      row_side_colors = ann_col,
      Rowv = cluster_row,
      Colv = cluster_col,
      #row_dend_left = T,
      show_rownames = F,
      show_colnames = T,
      main = strTitle
    )
    return(p)
  }
