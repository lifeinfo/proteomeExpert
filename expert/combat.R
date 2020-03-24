#mycombat<-function(peptide,batchf,psep="\t",pheader=T,bheader=T,bsep="\t"){
mycombat<-cmpfun(function(peptide,batch){
  
  options(stringsAsFactors = F)
  #peptide<-read.table(peptidef,header = pheader,sep = psep)
  #batch<-read.table(batchf,header = bheader,sep = bsep)
  rownames(batch)<-batch[,1]
  
  mydata<-peptide[,-c(1,2)]
  rownames(mydata)<-peptide[,1]
  mydata[is.na(mydata)]<-0
  combat.peptide<-ComBat(data.matrix(mydata),batch=as.factor(batch[,2]),mod = NULL)
  combat.peptide<-data.frame(combat.peptide)
  combat.peptide[is.na(peptide[,-c(1:2)])]<-NA
  combat.peptide<-cbind(peptide[,1:2],combat.peptide)
  return(combat.peptide)
})