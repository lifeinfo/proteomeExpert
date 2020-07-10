myVolcano<-cmpfun(function(data,adjp_threshold=0.05,fc_threshold=2){
  plot(data$log2fc,-log10(data$adjp),col="#00000033",pch=19,las=1,
       xlab="Log2(FC)",
       ylab="-log10 adjusted P value"
  )  
  up<-subset(data,data$adjp<adjp_threshold&data$log2fc>log2(fc_threshold))
  points(up[,1],-log10(up[,3]),col=1,bg=brewer.pal(9,"YlOrRd")[6],pch=21,cex=1.5)
  #text(up[,1],-log10(up[,2]),rownames(up),adj = -0.1)
  down<-subset(data,data$adjp<adjp_threshold&data$log2fc<(-log2(fc_threshold)))
  points(down[,1],-log10(down[,3]),col=1,bg=brewer.pal(11,"RdBu")[9],pch=21,cex=1.5)
  abline(h=-log10(adjp_threshold),v=c(-log2(fc_threshold),log2(fc_threshold)),lty=2,lwd=1)
})

myVolcanoData<-cmpfun(function(data,adjp_threshold=0.05,fc_threshold=2){
  up<-subset(data,data$adjp<adjp_threshold&data$log2fc>log2(fc_threshold))
  down<-subset(data,data$adjp<adjp_threshold&data$log2fc<(-log2(fc_threshold)))
  df<-rbind(up,down)
  df$p <- format(df$p,digits = 3, scientific = T)
  df$adjp <- format(df$adjp,digits = 3, scientific = T)
  df$regulation<-rep(c("up","down"),c(nrow(up),nrow(down)))
  return(df)
})
