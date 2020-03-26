options(stringsAsFactors = F)
##############################
pepstat <- function(d) {
  tgs <- length(d$peptide_group_label)
  prot_groups <- length(unique(d$prot))
  protetrypic <- length(grep("^1/", unique(d$prot)))
  return(
    data.frame(
      transition_groups = tgs,
      protein_groups = prot_groups,
      protetrypic = protetrypic
    )
  )
}
###########format peptide data matrix tg,prot,int; tech_rep_f:no header, sample name,replicate label
auto_preprocess <-
  function(filename = "peptides.txt",
           tech_rep_f = NULL,
           batchf = NULL,
           psep = "\t",
           tsep = "\t",
           pheader = TRUE,
           theader = FALSE,
           bheader = TRUE,
           bsep = "\t",
           isLR = TRUE) {
    t1 <- proc.time()
    cat("1: ", proc.time() - t1)
    pep.data <-
      read.table(
        filename,
        header = pheader,
        sep = psep,
        check.names = F,
        quote = "",
        fill = TRUE
      )
    incProgress(1/5,message = "Data read completed! Starting to do normalizaion and precursor selection...")
    pep.data[is.na(pep.data)]<-NA
    pep.data<-pep.data[complete.cases(pep.data[,1]),]
    pep.data <- pep.data[!grepl("^1/CON", pep.data[, 2], fixed = F), ]
    
    pep.data[pep.data == 0] <- NA
    pep <- as.vector(as.matrix(pep.data[, 3:ncol(pep.data)]))
    #print(paste("missing rate is: ",sum(is.na(pep))/length(pep),sep=""))
    
    #log2 transform
    cat("2", proc.time() - t1)
    pep.data.log2 <- pep.data
    rownames(pep.data.log2) <- pep.data.log2[, 1]
    pep.data.log2 <- pep.data.log2[, -1]
    pep.data.log2[, 2:ncol(pep.data.log2)] <-
      log2(as.matrix(pep.data.log2[, 2:ncol(pep.data.log2)]))
    if(ncol(pep.data.log2)==2){
      pep.data.log2.group<-split(pep.data.log2,pep.data.log2[,1])
      prot.t<-lapply(pep.data.log2.group, function(x){
        x<-x[order(x[,2],decreasing = T),]
        y<-0
        if(nrow(x)>3){
          y<-mean(x[1:3,2],na.rm = T)
        }else{
          y<-mean(x[1:nrow(x),2],na.rm = T) 
        }
        return(y)
      })
      prot.t.d<-do.call(rbind,prot.t)
      prot.d<-data.frame(prot=rownames(prot.t.d),intensity=prot.t.d[,1])
      colnames(prot.d)[2]<-colnames(pep.data)[3]
      return(prot.d)
    }
    cat("3", proc.time() - t1)
    #R preprocessCore normalize.quantiles()
    
    pep.data.log2.qn = normalize.quantiles(as.matrix(pep.data.log2[, 2:ncol(pep.data.log2)]))
    colnames(pep.data.log2.qn) <- colnames(pep.data.log2)[-1]
    rownames(pep.data.log2.qn) <- rownames(pep.data.log2)
    #write.table(pep.data.log2.qn, "data/qn_log2_pep.txt",col.names=T,row.names=T,quote = F,sep = "\t",na = "NA")
    ####technical imputation
    #no technical imputaiton
    if(is.null(tech_rep_f)){
      data.tech.rep<-cbind(pep.data[,1:2],pep.data.log2.qn)
    }
    ##technical imputation
    else{    
      #read annotation
      anno <-
        read.table(tech_rep_f,
                   header = theader,
                   sep = tsep,
                   check.names = F)
      if(nrow(anno)!=ncol(pep.data.log2.qn))
        return("Number of samples do not match in peptide file and technical replicate file!")
      else if(length(intersect(unlist(anno[,1]),colnames(pep.data.log2.qn)))!=nrow(anno)){
        return("Samples do not match in peptide file and technical replicate file!")
      }
      else{
        
      }
      colnames(anno) <- c("V1", "V2")
      rownames(anno) <- anno$V1
      anno <- anno[colnames(pep.data.log2.qn), ]
      replicates <- split(as.vector(anno$V1), as.factor(anno$V2))
      replicates_impute <- function(x) {
        index <- which(is.na(x))
        x.median <- median(x, na.rm = T)
        x[index] <- x.median
        return(x)
      }
      cat("4", proc.time() - t1)
      data <- pep.data.log2.qn
      
      data.reps <- data.frame()
      for (reps in replicates) {
        if (length(reps) > 1) {
          d <- apply(data[, reps], 1, function(x) {
            return(replicates_impute(x))
          })
        }
        else {
          d <- data[, reps]
          d <- t(as.matrix(d))
          rownames(d) <- reps
        }
        data.reps <- rbind(data.reps, d)
      }
      cat("5", proc.time() - t1)
      data.tech.rep <- t(data.reps)
      data.tech.rep[is.nan(data.tech.rep)] <- NA
      data.tech.rep <- cbind(pep.data[, 1:2], data.tech.rep)
      #write.table(data.tech.rep, "data/data.tech.rep.txt",col.names=T,row.names=F,quote = F,sep = "\t",na = "NA")
    }
    
    cat("6", proc.time() - t1)
    if (!is.null(batchf)) {
      #print(batchf)
      
      batch <- read.table(batchf, header = bheader, sep = bsep)
      if(nrow(batch)!=ncol(data.tech.rep)-2)
        return("Number of samples do not match in peptide file and batch file!")
      else if(length(intersect(unlist(batch[,1]),colnames(data.tech.rep[,-c(1:2)])))!=nrow(batch)){
        return("Samples do not match in peptide file and batch file!")
      }else{
        data.tech.rep <- mycombat(data.tech.rep, batch)
        temp<-data.tech.rep
        temp<-data.tech.rep[,-c(1,2)]
        temp[temp<0]<-NA
        data.tech.rep[,-c(1,2)]<-temp
        rm(temp)
      }


    }
    
    ###order
    
    data <- data.tech.rep
    colnames(data)[1:2] <- c("tg", "prot")
    cat("7", proc.time() - t1)
    n = ncol(data)
    pep2 <- apply(data[, -c(1, 2)], 1, function(x) {
      NAs <- length(which(is.na(x)))
      meanexpr1 <- sum(as.numeric(x), na.rm = TRUE) / (n - NAs)
      meanexpr2 <- sum(as.numeric(x), na.rm = TRUE) / n
      d <- c(NAs, meanexpr1, meanexpr2)
      return(d)
    })
    pep2 <- t(pep2)
    colnames(pep2) <- c("NAs", "meanexpr1", "meanexpr2")
    pep_expr = cbind(data[, 1], pep2, data[, c(-1)])
    cat("8", proc.time() - t1)
    ########################################threee order methods ##################just choose only one
    ##order by pg ,#NA,intesity
    pep_order = pep_expr[order(pep_expr[, 5], pep_expr[, 2], -pep_expr[, 3]), ]
    colnames(pep_order)[1] <- "tg"
    ##order by pg intensity,#NA(excluding NAs)
    # pep_order=pep_expr[order(pep_expr[,5],-pep_expr[,3],pep_expr[,2]),]
    # colnames(pep_order)[1]<-"tg"
    # ###order by pg intensity,#NA(all samples)
    # pep_order=pep_expr[order(pep_expr[,5],-pep_expr[,4],pep_expr[,2]),]
    # colnames(pep_order)[1]<-"tg"
    
    #######################################################################################################
    pep_order2 <- pep_order[, c(-2, -3, -4)]
    
    ###select top 3 pep
    pre.prot = ""
    same.count = 0
    pep_order2.top3 <- data.frame()
    for (i in 1:nrow(pep_order2)) {
      if (pre.prot == as.vector(pep_order2[i, "prot"])) {
        same.count = same.count + 1
      }
      else {
        pre.prot = as.vector(pep_order2[i, "prot"])
        same.count = 1
      }
      if (same.count <= 3) {
        pep_order2.top3 <- rbind(pep_order2.top3, pep_order2[i, ])
      }
    }
    cat("9", proc.time() - t1)
    ##protetypic proteins are saved
    # pep_order2.top3 <-
    # pep_order2.top3[grep("^1/", pep_order2.top3$prot), ]
    
    pep_order2.top3 <-
      pep_order2.top3[c("prot", "tg", colnames(pep_order2.top3)[3:ncol(pep_order2.top3)])]
    pep_order2.top3[pep_order2.top3 == 0] <- NA
    if(!isLR){
      incProgress(1/2,message = "Protein inferencing using mean of top 3!")
      
      top3.prot.group<-split(pep_order2.top3,pep_order2.top3$prot)
      top3.mean<-lapply(top3.prot.group, function(l){
        apply(l[,3:ncol(l)],2,mean,na.rm=T)
      })
      
      top3.mean<-do.call(rbind,top3.mean)
      top3.mean<-cbind(prot=rownames(top3.mean),top3.mean)
      incProgress(8/10,message = "Protein matrix completed!")
      top3.mean[is.na(top3.mean)]<-''
      return(top3.mean)
    }
    #write.table(pep_order2.top3, paste("data/",Sys.Date(),"pep.top3.txt",sep = ""),row.names = F,  quote = F,sep = "\t",na = "NA")
    cat("10", proc.time() - t1)
    #############lr for pep2prot
    incProgress(1/2,message = "Protein inferencing using LR!")
    prot.matrix <- pep2prot(pep_order2.top3)
    prot.matrix[, -c(1:2)] <- round(prot.matrix[, -c(1:2)], 2)
    prot.matrix <- prot.matrix[, -2]
    incProgress(8/10,message = "Protein matrix completed!")
    return(prot.matrix)
    cat("11", proc.time() - t1)
  }

#t<-auto_preprocess("\\\\172.16.13.5\\sky\\workspace\\r\\common\\data\\data.tech.rep.txt","\\\\172.16.13.5\\sky\\workspace\\r\\common\\data\\tech.txt")
#tt<-auto_preprocess("E:/temp/F20190522caix_pep_helaReport.txt","E:/temp/tech_rep_file.txt",theader = TRUE)