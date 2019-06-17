drawVolcano <- function(data,Label,labelA,labelB,Foldchange=1.5,pvalue=0.05,adjust=FALSE, pdfPath,strTitle="volcano plot",outFile=NULL){ 
df8 <- data.frame()
df8 <- 2^data
df8[is.na(df8)] <- 0
H_set <- which(Label==labelA)
L_set <- which(Label==labelB)
df8$fd <- apply(df8,1, function(x) log2((mean(x[H_set],na.rm = T)/mean(x[L_set],na.rm = T))))
x<-c(0.0,0.0)
df9 <- data
df9[is.na(df9)]<-0

x <- apply(df9, 1, function(v){t.test(v[H_set],v[L_set], paired = F, var.equal = F)$p.value})
if(adjust==FALSE){
  df8$P_value <- x
}else{
  df8$P_value <- p.adjust(x, method="BH")
}


pdf(pdfPath)
plot(df8$fd, -log10(VALUE), col="#00000033", pch=19,
     xlab=paste("log2 (fold change)"),
     ylab="-log10 (P_value)",
     main=paste0(strTitle,"£º",labelA,"&",labelB))

up <- subset(df8, df8$P_value < pvalue & df8$fd > Foldchange)
down <- subset(df8, df8$P_value< pvalue & df8$fd< -Foldchange)
# write.csv(up,file = "volcano/H_L_up.csv")
# write.csv(down,file = "volcano/H_L_down.csv")
points(up$fd, -log10(up$P_value), col=1, bg = brewer.pal(9, "YlOrRd")[6], pch=21, cex=1.5)
points(down$fd, -log10(down$P_value), col = 1, bg = brewer.pal(11,"RdBu")[9], pch = 21,cex=1.5)
abline(h=-log10(VALUE),v=c(-Foldchange,Foldchange),lty=2,lwd=1)

dev.off()
}
