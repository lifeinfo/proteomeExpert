drawcorrplot <- function(data){ 
  df_cor <- data
  mycor=cor(df_cor, use = "pairwise.complete.obs")			
  corrplot(mycor,type = "upper",tl.col = "black",tl.srt = 45, tl.cex = 0.5)
}
