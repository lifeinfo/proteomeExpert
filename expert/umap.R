drawUMAP <- function(data,label, strTitle="UMAP",rowNormalization=T,colNormalization=F){
  DF <- data.frame(t(data))
  DF$label <- label
  M1 <- DF
  if(!'label' %in% colnames(M1)){
    print('A column with named label must existed in data frame')
    return(NULL)
  }
  tmp <- M1[,colnames(M1)!='label']
  if(rowNormalization){
    tmp <- data.frame(t(apply(tmp,1,function(v){(v-mean(v,na.rm=T))/sd(v,na.rm=T)})),stringsAsFactors=F)
    rownames(tmp) <- rownames(M1)
  }
  if(colNormalization){    tmp <- apply(tmp,2,function(v){(v-mean(v,na.rm=T))/sd(v,na.rm=T)})   }
  tmp[is.na(tmp)] <- 0
  obj = umap(d=tmp,method='naive')
  clnames <- colnames(tmp)
  df1 <- data.frame(obj$layout)
  df1$label <- M1$label
  colnames(df1) <- c('X','Y','label')
  
  p <- ggplot(df1, aes(x=X, y=Y, colour=label)) + geom_point(size=4)
  p <- p + theme(  panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_blank(),
                   plot.title   = element_text(size=16),
                   axis.line.x = element_line(color="black", size = 0.5),
                   axis.line.y = element_line(color="black", size = 0.5),
                   panel.background = element_blank())
  
  #strLabx <- sprintf("PC1(%4.2f%%)",percentages[1]*100)
  p <- p +  labs(title =strTitle)
  #p <- p +   scale_colour_manual(values=ptColors)
  return(p)
}