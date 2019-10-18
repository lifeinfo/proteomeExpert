options(stringsAsFactors = F)
setwd("Y:/data/project/proteomeExpert/app")
protM<-read.table("demo/prot.txt",sep = "\t",header = T,row.names = 1)
sample<-read.csv("./demo/sampleInfo.csv",header = T)
individual<-read.csv("./demo/IndividualInfo.csv",header = T)
anno<-merge(sample,individual,by="Individual_ID")
#note suppose  the sample names are one-to-one correspondence 
#myList<-list(data=protM,label=anno$TissueType)

qc_label<-anno$TissueType
data<-protM
for (v in unique(qc_label)) {
  assign(paste0(v,".df"),data[which(qc_label==v),])
  assign(paste0(v,".nna"),apply(get(paste0(v,".df")), 1, function(x){sum(!is.na(x))}))
  for (i in 1:9) {
     assign(paste0(v,"ratio.",i),get(paste0(v,".df"))[get(paste0(v,".nna"))/ncol(get(paste0(v,".df")))>=i/10,])
     
  }
}
