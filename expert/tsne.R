drawTSNE <- function(data,label,rowNormalization=F,colNormalization=F,perplexity=10,strTitle='tSNE'){
  DF <- data.frame(t(data))
  DF$label <- label
  M <- DF[,colnames(DF)!='label']
  if(rowNormalization){M <- data.frame(t(apply(M,1,function(v){(v-mean(v,na.rm=T))/sd(v,na.rm=T)})))}
  if(colNormalization){M <- apply(M,2,function(v){(v-mean(v,na.rm=T))/sd(v,na.rm=T)})}
  M[is.na(M)] <- 0
  indx <- match('label',colnames(M))
  clnames <- colnames(DF)[colnames(DF)!='label']
  tsn = tsne(M,perplexity =perplexity)
  cnames <- colnames(M)
  tsn <- data.frame(tsn,DF$label)
  colnames(tsn) <- c("X","Y","label")
  rownames(tsn) <- rownames(M)
  #tsn <- tsn[-c(which(tsn$X==min(tsn$X)),which(tsn$Y==min(tsn$Y))),]
  #tsn <- tsn[-c(which(tsn$X==max(tsn$X)),which(tsn$Y==max(tsn$Y))),]
  #lblColors <- c(N='#537e35',M='#e17832',A='#f5b819',C='#5992c6',P='#282f89',W='mediumorchid3')
  #lblColors <- c(A='#537e35',M='#e17832',N='#f5b819',B='#5992c6',C='#282f89',W='mediumorchid3')
  p <- ggplot(tsn, aes(x=X, y=Y, colour=label)) + geom_point(size=4)
  p <- p + theme(  panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_blank(),
                   plot.title = element_text(size=15),
                   axis.line.x = element_line(color="black", size = 0.5),
                   axis.line.y = element_line(color="black", size = 0.5),
                   panel.background = element_blank())
  p <-  p +  labs(title=strTitle)
  #p <- p +   scale_colour_manual(values=ptColors)
  return(p)
}

