drawcorrplot <- cmpfun(function(data) {
  df_cor <- data
  mycor = cor(df_cor, use = "pairwise.complete.obs")
  p <-
    corrplot(
      mycor,
      method = "number",
      type = "upper",
      tl.col = "black",
      tl.srt = 45,
      tl.cex = 0.8,
      cl.cex = 0.5
    )
  return(p)
})
