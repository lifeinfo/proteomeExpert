library(caret)

####feature filter
featureFilter<-function(label_protM,methods,fs_missing_ratio){
  
  print(methods)
  variance=methods[1]
  correlation=methods[2]
  print("----------------")

  protM<-label_protM[,-which(colnames(label_protM)=="label")]
  print(dim(protM))
  
  protM<-apply(protM,2,as.numeric,na.rm=T)
  #filter by missing ratio
  fs_missing_ratio<-as.numeric(fs_missing_ratio)
  print(fs_missing_ratio)
  if(fs_missing_ratio>=0 & fs_missing_ratio<=1){
      fna_ratio<-apply(protM,2,function(x){
      sum(is.na(x))/length(x)
      })
     fna_ratio_vars<-which(fna_ratio<=fs_missing_ratio)
     protM<-protM[,fna_ratio_vars]
     #label_protM<-label_protM[,fna_ratio_vars]
  }
  print(dim(protM))

  #filter near zero 
  if(variance){
      near_zero_vars <- nearZeroVar(protM)
      if(length(near_zero_vars)>0)
        protM <- protM[,-near_zero_vars]
  #label_protM<-label_protM[,-near_zero_vars]
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
      if(length(high_Corr_vars)>0)
      protM <- protM[, -high_Corr_vars]
  }

  print(dim(protM))
  label_protM2<-cbind(label=label_protM[,"label"],protM)
  return(label_protM2) 
}

##########################################
################ feature selection #######################
splitLabelMatrix<-function(label_protM){
  label<-label_protM[,"label"]
  #label<-as.factor(label)
  protM<-label_protM[,-which(colnames(label_protM)=="label")]
  return(list(label=lable,protM=protM))
}
featureSel<-function(label_protM,featureSel_algorithm,nfeatures=50){
  #print(myhead(label_protM))

  switch (featureSel_algorithm,
          random_forest = fsRf(label_protM,nfeatures),
          lasso = fsLasso(label_protM)
  )
}
############################## random froest ####################
fsRf<-function(label_protM,nfeatures){
    label<-label_protM[,"label"]
    label<-as.factor(label)
    protM<-label_protM[,-which(colnames(label_protM)=="label")]
    #stringsAsFactors=F
    protM[is.na(protM)]<-0
    protM<-as.data.frame(apply(protM,2,as.numeric,na.rm=T))
    
    #protM<-as.data.frame(protM)
    # near_zero_vars <- nearZeroVar(protM)
    # if(length(near_zero_vars)>0)
    #   protM <- protM[,-near_zero_vars]
    Processed_protM <- preProcess(protM)
    Processed_protM <- predict(Processed_protM, protM)
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
    if(length(profile$fit$forest$xlevel)<nfeatures)
      nfeatures<-length(profile$fit$forest$xlevel)
    features<-names(profile$fit$forest$xlevel[1:nfeatures])
    #print(colnames(label_protM))
    #print(features)
    return(label_protM[,c("label",features)])
  }
################################# lasso
fsLasso<-function(label_protM){
  label<-label_protM[,"label"]
  protM<-label_protM[,-which(colnames(label_protM)=="label")]
  cvglm <- cv.glmnet(as.matrix(protM),label, family = "multinomial", nfold = 10, type.measure = "class", paralle = TRUE, standardize=T,alpha = 1)
  glm_multi <- glmnet(as.matrix(protM),label, family = "multinomial", lambda = cvglm$lambda.1se, alpha = 1)
  glm_multi_coef<-coef(glm_multi)[[1]][-1,]
  #names(glm_multi_coef)[glm_multi_coef!=0]
  return(names(glm_multi_coef)[glm_multi_coef!=0])
}
