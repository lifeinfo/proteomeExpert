drawPCA <-
  function(data,
           label,
           rowNormalization = F,
           colNormalization = F,
           strTitle = NULL) {
    ## M is a matrix or dataframe, rows are samples and columns are features, rownames are sample names
    DF <- data.frame(t(data))
    DF$label <- label
    M <- DF[, colnames(DF) != 'label']
    if (rowNormalization) {
      M <-
        data.frame(t(apply(M, 1, function(v) {
          (v - mean(v, na.rm = T)) / sd(v, na.rm = T)
        })))
      #print('Normalization by row is done!')
    }
    if (colNormalization) {
      M <- apply(M, 2, function(v) {
        (v - mean(v, na.rm = T)) / sd(v, na.rm = T)
      })
    }
    clnames <- colnames(DF)[colnames(DF) != 'label']
    M[is.na(M)] <- 0
    m1 <- prcomp(M, colNormalization)
    
    Y  <- scale(M, m1$center, m1$scale) %*% m1$rotation
    Y  <- Y[, c(1, 2)]
    
    Y <- data.frame(Y, DF$label)
    
    colnames(Y) <- c("PC1", "PC2", "label")
    if (is.null(strTitle)) {
      strTitle <- sprintf("PCA:%d features", length(clnames))
    }
    eigs <- m1$sdev ^ 2
    percentages <- eigs[1:2] / sum(eigs)
    # lblColors <- c(N='#537e35',M='#e17832',A='#f5b819',C='#5992c6',P='#282f89',W='mediumorchid3')
    #lblColors <- c(training='#537e35',validation='#e17832',A='#f5b819',C='#5992c6',P='#282f89',W='mediumorchid3')
    p <-
      ggplot(Y, aes(x = PC1, y = PC2, colour = label)) + geom_point(size = 4)
    p <- p + theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line.x = element_line(color = "black", size = 0.25),
      axis.line.y = element_line(color = "black", size = 0.25),
      plot.title   = element_text(size = 16),
      panel.background = element_blank()
    )
    
    strLabx <- sprintf("PC1(%4.2f%%)", percentages[1] * 100)
    p <-
      p +  labs(
        x = strLabx,
        y = sprintf("PC2(%4.2f%%)", percentages[2] * 100),
        title = strTitle
      )
    #p <- p +   scale_colour_manual(values = c("blue", "red", "black"))
    
    return(p)
  }
