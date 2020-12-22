drawheatmap <-
  cmpfun(function(data,
           Label,
           strTitle = "Heatmap",
           cluster_row = T,
           cluster_col = T) {
    df11 <- as.data.frame(data)
    #df11[is.na(df11)] <- 0
    ann_col <- data.frame(type = Label, row.names = colnames(df11))
    ann_col$type <- factor(ann_col$type)

    p <- pheatmap(
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
    return(p)
  })