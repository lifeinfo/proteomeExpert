library(caret)

####feature filter
featureFilter<-function(label_protM,methods){
  print(methods)
  variance=methods[1]
  correlation=methods[2]
  print("----------------")

  protM<-label_protM[,-which(colnames(label_protM)=="label")]
  print(dim(protM))
  
  protM<-apply(protM,2,as.numeric,na.rm=T)

  #filter near zero 
  if(variance){
      near_zero_vars <- nearZeroVar(protM)
  protM <- protM[,-near_zero_vars]
  label_protM<-label_protM[,-near_zero_vars]
  }
  print(dim(protM))
  print(correlation)
  #filter high correlation
  #pay attentaion to sd equal 0, if will turn out errors
  if(correlation){
      protM_t<-protM
      protM_t[is.na(protM_t)]<-0
      corr_vars <- cor(protM_t,use ='pairwise.complete.obs' )
      high_Corr_vars <- findCorrelation(corr_vars, 0.90)
      protM <- protM[, -high_Corr_vars]
  }

  print(dim(protM))
  label_protM2<-cbind(label=label_protM[,"label"],protM,stringsAsFactors = FALSE)
  return(label_protM2) 
}

##########################################
################ feature selection #######################

featureSel<-function(label_protM,rf,nfeatures){
  label<-label_protM[,"label"]
  protM<-label_protM[,-which(colnames(label_protM)=="label")]
  ############################## random froest ####################
  if(rf){
    Processed_protM <- preProcess(protM)
    Processed_protM <- predict(Process, Processed_protM)
    data.filter <- sbf(Processed_protM,label,
                       sbfControl = sbfControl(functions=rfSBF,
                                               verbose=F,
                                               method='cv'))
    x <- Processed_protM[data.filter$optVariables]
    profile <- rfe(x,label,
                   #sizes = c(3,5,8,12,15,20),
                   rfeControl = rfeControl(functions=rfFuncs
                                           ,method='cv'))
    #plot(profile,type=c('o','g'))
    features<-names(profile$fit$forest$xlevel[1:nfeatures])
    return(label_protM[,c("label",features)])
  }
  #################################end
}