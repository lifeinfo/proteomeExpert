xgboost_classfier_training<-function(trainX,trainY,parameters, numRounds){
  
  num <- length(levels(trainY))
  if(num > 2){
    parameters[["objective"]] <- 'multi:softprob'
    parameters[["num_class"]] <- num
  }
  labelY <- as.numeric(trainY) - 1
  xg_train <- xgb.DMatrix(data = as.matrix(trainX), label = labelY)
  
  # bst <- xgb.train(
  bst <- xgboost::xgboost(
    params = parameters,
    data = xg_train,
    nrounds = numRounds
  )
  return(bst)

  
}

xgboost_classfier_predict <-function(xgb_model, testdata)
{
  dtestset <- data.matrix(testdata)
  dtest<- xgb.DMatrix(dtestset)
  pred <- predict(xgb_model,dtest)
  return(pred)
}

#format input protein matrix
formatProteinMatrix <- function(proteinData)
{
  sampleNames <- colnames(proteinData)
  sampleNames <- sampleNames[-1]
  attrNames <- as.matrix(proteinData[, 1])
  attrNames <- as.vector(attrNames[, 1])
  #proteinData <-  data.matrix(proteinData)
  proteinData <-  proteinData[, -1]
  proteinData <- as.data.frame(t(proteinData))
  colnames(proteinData)  <-  attrNames
  #row.names(proteinData) <- sampleNames
  proteinData[is.na(proteinData)] <- 0
  return(proteinData)
  
}
#xgboost中的result还原为原始的字符串值
formatXgbResult <- function(result, factoredLabel, sampleNames)
{
  result <- round(result, digits = 2)
  nclass <- length(levels(factoredLabel))
  idxVector <- c()
  predictedClass <- c()
  if( nclass > 2){
    nrow = length(result)/nclass
    result <- matrix(result, nrow = nrow,ncol = nclass, byrow = TRUE,  dimnames = list(sampleNames, levels(factoredLabel)))
    for (rowIdx in 1:nrow ){
      arow <- result[rowIdx,]
      idxVector <-c(idxVector, which.max(arow))
    }
    # for (idx in idxVector) {
    #   predictedClass <- c(predictedClass, levels(factoredLabel)[idx])
    # }
    
  }else{
    result <- matrix(result, nrow = length(result),ncol = 1, byrow = TRUE, dimnames = list(sampleNames,c("prob") ))
    acol <- result[,1]
    idxVector <- ceiling(acol+0.5)
    # for ( prob in acol) {
    #   idx <- 1
    #   if(prob > 0.5 ){
    #     idx <- 2
    #   }
    #   predictedClass <- c(predictedClass, levels(factoredLabel)[idx])
    # }
    #result <- cbind(result, predictedClass)
  }
  predicted  <- levels(factoredLabel)[idxVector]
  result <- cbind(result, predicted )
  print(result)
  return( result)
}
