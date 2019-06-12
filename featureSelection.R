library(caret)

####feature filter
featureFilter<-function(label_protM){
  protM<-label_protM[,-c("label")]
  #filter near zero 
  near_zero_vars <- nearZeroVar(protM)
  protM <- protM[,-near_zero_vars]
  label_protM<-label_protM[,-near_zero_vars]
  #filter high correlation
  corr_vars <- cor(protM)
  high_Corr_vars <- findCorrelation(corr_vars, 0.90)
  protM <- protM[, -high_Corr_vars]
  label_protM2<-cbind(label_protM[,"label"],protM)
  return(label_protM2) 
}

##########################################
################ feature selection #######################

featureSel<-function(label_protM,rf,nfeatures){
  label<-label_protM[,"label"]
  protM<-label_protM[,-c("label")]
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