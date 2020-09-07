missing_plot <- function(df) {
  cbbPalette <-
    c(
      #"#000000",
      "#E69F00",
      "#56B4E9",
      "#009E73",
      "#F0E442",
      "#0072B2",
      "#D55E00",
      "#CC79A7"
    )
  data <- df
  rNA = matrix(0, nrow(data), 1)
  
  system.time(rNA[, 1] <-
                apply(data, 1, function(x) {
                  length(which(is.na(x)))
                }))
  cNA = matrix(0, ncol(data), 1)
  
  system.time(cNA[, 1] <-
                apply(data, 2, function(x) {
                  length(which(is.na(x)))
                }))
  
  
  
  #plot
  cna <- as.vector(cNA[, 1]) / nrow(rNA)
  rna <- as.vector(rNA[, 1]) / nrow(cNA)
  cna.cut <- cut(cna, breaks = seq(0, 1, by = 0.1))
  
  rna.cut <- cut(rna, breaks = seq(0, 1, by = 0.1))
  #print("Results")
  mymagnify = 1.5
  layout(matrix(c(1:4, 2, 3), 3))
  plot(
    density(cna),
    main = "Density plot of missing rate by column",
    bty="n",xlim = c(0,1),
    col = cbbPalette[2],
    cex = mymagnify,
    cex.lab = mymagnify,
    cex.axis = mymagnify,
    cex.main = mymagnify
  )
  barplot(
    cNA[, 1],
    main = "missing values for column",
    xlab = "column index",
    ylab = "#of missing values",
    col = cbbPalette[2],
    cex = mymagnify,
    cex.lab = mymagnify,
    cex.axis = mymagnify,
    cex.main = mymagnify
  )
  barplot(
    rNA[, 1],
    main = "missing values for row",
    xlab = "row index",
    ylab = "#of missing values",
    col = cbbPalette[3],
    cex = mymagnify,
    cex.lab = mymagnify,
    cex.axis = mymagnify,
    cex.main = mymagnify
  )
  plot(
    density(rna),
    main = "Density plot of missing rate by row",
    bty="n",xlim = c(0,1),
    col = cbbPalette[3],
    cex = mymagnify,
    cex.lab = mymagnify,
    cex.axis = mymagnify,
    cex.main = mymagnify
  )
  
}
