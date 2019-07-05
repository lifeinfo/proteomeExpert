dataPreprocess<-function(d,replace_value,log_base,normaliztion){
  rownames(d)<-d[,1]
  d<-d[,-1]
  d<-data.matrix(d)
  #d<-apply(d,2,as.numeric,na.rm=T)

  if(log_base!="none"){
    d<-log(d,eval(parse(text = log_base)))
  }
  
  if(replace_value!="none"){
    switch(replace_value,
           "1" = d[is.na(d)]<-1,
           "0" = d[is.na(d)]<-0,
           minimum = d[is.na(d)]<-min(d,na.rm = T),
           "0.1" = d[is.na(d)]<-0.1*min(d,na.rm = T)
    )  
  }
  
  if(normaliztion!="none"){
    switch(normaliztion,
           quantile = d<-normalize.quantiles(q),
           zscore = d<-scale(d),
           maxmin = maxmin(d)
    )
  }
  return(d)
}

maxmin<-function(d){
  return(d)
}