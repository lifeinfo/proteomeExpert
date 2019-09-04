dataPreprocess<-function(d,replace_value,log_base,normaliztion,batch,DPTR,DPTechnicalRepMethod,DPBR,DPBiologicalRep){
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
           quantile = d<-myqn(d),
           zscore = d<-scale(d),
           maxmin = d<-maxmin(d)
    )
  }
  ##combat
  if(!is.null(batch)){
    t<-d
    t[is.na(t)]<-0
    combat.t<-ComBat(data.matrix(t),batch=as.factor(batch),mod = NULL)
    combat.t<-data.frame(combat.t)
    combat.t[is.na(d)]<-NA
    d<-combat.t
    rm(t)
    rm(combat.t)
    
  }
  ###technical replica
  if(!is.null(DPTR) & DPTechnicalRepMethod!="none"){
    d.temp<-data.frame(d)
    technical.group<-split(1:ncol(d),DPTR)
    d.tech<-data.frame()
    d.tech.col<-NULL
    for (tg in technical.group) {
      d.tech<-rbind(d.tech,apply(d.temp[tg],1,DPTechnicalRepMethod,na.rm=T))
      d.tech.col<-c(d.tech.col,colnames(d.temp[tg])[1])
    }
    rm(d.temp)
    d.tech<-t(d.tech)
    colnames(d.tech)<-d.tech.col
    rownames(d.tech)<-rownames(d)
    d<-d.tech
  }
 ###biological replica
  if(!is.null(DPBR) & DPBiologicalRep!="none"){
    if(!is.null(DPTR) & DPTechnicalRepMethod!="none"){
      del_index<-which(duplicated(DPTR))
      DPBR<-DPBR[-del_index]
    }
    d.temp<-data.frame(d)
    biological.group<-split(1:ncol(d),DPBR)
    d.bio<-data.frame()
    d.bio.col<-NULL
    for (bg in biological.group) {
      d.bio<-rbind(d.bio,apply(d.temp[bg],1,DPBiologicalRep,na.rm=T))
      d.bio.col<-c(d.bio.col,colnames(d.temp[bg])[1])
    }
    rm(d.temp)
    d.bio<-t(d.bio)
    colnames(d.bio)<-d.bio.col
    rownames(d.bio)<-rownames(d)
    d<-d.bio
  }
  
  
  print(dim(d))
  d<-round(d,2)
  return(d)
}
myqn<-function(d){
  
  d2<-normalize.quantiles(d)
  colnames(d2)<-colnames(d)
  rownames(d2)<-rownames(d)
  return(d2)
}
maxmin<-function(d){
  d <- data.matrix(d)
  center <- sweep(d, 2, apply(d, 2, min),'-')
  R <- apply(d, 2, max) - apply(d,2,min)
  d<- sweep(center, 2, R, "/")
  d <- data.frame(d)
  return(d)
}