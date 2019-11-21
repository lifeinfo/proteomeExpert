options(stringsAsFactors = F)
mylm <- function(x, y) {
  mylr <- lm(y ~ x + 1)
  mylr.summary <- summary(mylr)
  p=10
  if(nrow(mylr.summary$coefficients)>1)
    p = mylr.summary$coefficients[2, "Pr(>|t|)"]
  
  return(
    list(
      mylr = mylr,
      rSqured = mylr.summary$r.squared,
      fstatistic = mylr.summary$fstatistic[1],
      p=p
    )
  )
}

pep2prot <- cmpfun(function(top3) {
  #f<-"\\\\172.16.13.5\\sky\\workspace\\r\\common\\data\\2018-09-05pep.top3.txt"
  #top3<-read.table(f,sep="\t",header = T)
  prot.group <- split(top3, top3$prot)
  prot.matrix <- lapply(prot.group, function(d) {
    Y <- d[1, ]
    if (is.null(d)) {
    }
    else if (nrow(d) < 2) {
      
    }
    else{
      for (i in 2:nrow(d)) {
        x <- d[i, -c(1:2)]
        y <- d[1, -c(1:2)]
        
        xy.index <- which((!is.na(x)) & (!is.na(y)))
        xy0.index <- which((!is.na(x)) & (is.na(y)))
        if (length(xy.index) < 4 | all(unlist(x[xy.index]) == unlist(y[xy.index]))) {
          next
        }
        lr.results <- mylm(unlist(x[xy.index]), unlist(y[xy.index]))
        if(is.na(lr.results$p)|is.na(lr.results$rSqured)){
          next
        }else if(lr.results$rSqured > 0.36 &
            lr.results$p < 0.05 & length(xy0.index) > 0 & length(xy.index) > 2) {
          tryCatch(
            y.predict <-
              predict(lr.results$mylr, new = data.frame(x = unlist(x[xy0.index]))),
            error = function (e) {
              print(d[i, ])
              stop()
            }
          )
          
          y.predict[y.predict < 4] <- NA
          Y[xy0.index + 2] <- y.predict
        }
      }
    }
    return(Y)
  })
  
  prot.matrix <- do.call(rbind, prot.matrix)
  return(prot.matrix)
})
