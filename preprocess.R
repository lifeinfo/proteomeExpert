

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
           tech_rep_f = "technical_rep.txt",
           batchf = 'F',
           psep = "\t",
           tsep = "\t",
           pheader = TRUE,
           theader = FALSE,
           bheader = TRUE,
           bsep = "\t") {
    pep.data <-
      read.table(
        filename,
        header = pheader,
        sep = psep,
        check.names = F,
        quote = "",
        fill = TRUE
      )
    pep.data <- pep.data[!grepl("^1/CON", pep.data[, 2], fixed = F), ]
    
    pep.data[pep.data == 0] <- NA
    pep <- as.vector(as.matrix(pep.data[, 3:ncol(pep.data)]))
    #print(paste("missing rate is: ",sum(is.na(pep))/length(pep),sep=""))
    
    #log2 transform
    pep.data.log2 <- pep.data
    rownames(pep.data.log2) <- pep.data.log2[, 1]
    pep.data.log2 <- pep.data.log2[, -1]
    pep.data.log2[, 2:ncol(pep.data.log2)] <-
      log2(as.matrix(pep.data.log2[, 2:ncol(pep.data.log2)]))
    
    #R preprocessCore normalize.quantiles()
    
    pep.data.log2.qn = normalize.quantiles(as.matrix(pep.data.log2[, 2:ncol(pep.data.log2)]))
    colnames(pep.data.log2.qn) <- colnames(pep.data.log2)[-1]
    rownames(pep.data.log2.qn) <- rownames(pep.data.log2)
    #write.table(pep.data.log2.qn, "data/qn_log2_pep.txt",col.names=T,row.names=T,quote = F,sep = "\t",na = "NA")
    
    #read annotation
    anno <-
      read.table(tech_rep_f,
                 header = theader,
                 sep = tsep,
                 check.names = F)
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
    data.tech.rep <- t(data.reps)
    data.tech.rep[is.nan(data.tech.rep)] <- NA
    data.tech.rep <- cbind(pep.data[, 1:2], data.tech.rep)
    #write.table(data.tech.rep, "data/data.tech.rep.txt",col.names=T,row.names=F,quote = F,sep = "\t",na = "NA")
    ###############no need:using python to do combat #########
    ################ignore here
    #######################################################################
    #print(batchf)
    if (!is.null(batchf)) {
      #print(batchf)
      
      batch <- read.table(batchf, header = bheader, sep = bsep)
      head(batch)
      data.tech.rep <- mycombat(data.tech.rep, batch)
    }
    
    ###order
    
    data <- data.tech.rep
    colnames(data)[1:2] <- c("tg", "prot")
    
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
    
    ##protetypic proteins are saved
    pep_order2.top3 <-
      pep_order2.top3[grep("^1/", pep_order2.top3$prot), ]
    pep_order2.top3 <-
      pep_order2.top3[c("prot", "tg", colnames(pep_order2.top3)[3:ncol(pep_order2.top3)])]
    pep_order2.top3[pep_order2.top3 == 0] <- NA
    #write.table(pep_order2.top3, paste("data/",Sys.Date(),"pep.top3.txt",sep = ""),row.names = F,  quote = F,sep = "\t",na = "NA")
    
    #############lr for pep2prot
    prot.matrix <- pep2prot(pep_order2.top3)
    prot.matrix[, -c(1:2)] <- round(prot.matrix[, -c(1:2)], 2)
    prot.matrix <- prot.matrix[, -2]
    return(prot.matrix)
    
  }

#t<-auto_preprocess("\\\\172.16.13.5\\sky\\workspace\\r\\common\\data\\data.tech.rep.txt","\\\\172.16.13.5\\sky\\workspace\\r\\common\\data\\tech.txt")
