#################################
# Function
#################################



#myhead function

myhead <- function(d) {
  if (ncol(d) > 6) {
    t = head(d[, 1:6])
  }
  else
    t = head(d)
  return(t)
}

# smoothScatter plot

drawsmooth <- function(data1, data2, strTitle = "smoothScatter plot") {
  r <- cor(data1, data2, use = "pairwise.complete.obs")
  smoothScatter(
    data1,
    data2,
    nrpoints = 100,
    cex = 2,
    colramp = colorRampPalette(c(blues9, "orange", "red")),
    main = strTitle,
    xlab = "repA",
    ylab = "repB"
  )
  abline(lm(data1 ~ data2),
         col = "red",
         lwd = 2,
         lty = 2)
  text(
    min(data1) * 1.3,
    max(data2) * 0.8,
    labels = paste0("r =", as.character(round(r, 4))),
    cex = 1.2
  )
}

# density plot
drawdensity <- function(data) {
  plot(density(na.omit(unlist(data))), main = "density default")
}