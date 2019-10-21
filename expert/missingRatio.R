countNa <- function(data, qc_label) {
  num <- nrow(data)
  df.nna <- data.frame(matrix(NA, 9, 0))
  for (v in unique(qc_label)) {
    data.v <- data[,which(qc_label == v)]
    data.v.nna <- apply(data.v, 1, function(x) {
      sum(!is.na(x))
    })
    vec.v.num <- c()
    for (i in 1:9) {
      vec.v.num <- c(vec.v.num, sum(data.v.nna / ncol(data.v) >= i / 10))
    }
    df.nna <- cbind(df.nna, vec.v.num)
    vec.v.ratio <- round(vec.v.num / num * 100, 2)
    df.nna <- cbind(df.nna, vec.v.ratio)
  }
  
  colnames(df.nna) <-
    paste0(rep(unique(qc_label), each = length(unique(qc_label))), c(".num", ".percent"))
  rownames(df.nna) <- paste0(seq(10, 90, 10), "%")
  return(df.nna)
}

# options(stringsAsFactors = F)
# setwd("Y:/data/project/proteomeExpert/app")
# protM <-
#   read.table(
#     "demo/prot.txt",
#     sep = "\t",
#     header = T,
#     row.names = 1
#   )
# sample <- read.csv("./demo/sampleInfo.csv", header = T)
# individual <- read.csv("./demo/IndividualInfo.csv", header = T)
# anno <- merge(sample, individual, by = "Individual_ID")
# #note suppose  the sample names are one-to-one correspondence
# #myList<-list(data=protM,label=anno$TissueType)
# qc_label <- anno$Type
# data <- protM
# countNa(data,qc_label)