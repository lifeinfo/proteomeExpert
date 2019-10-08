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
  
  #convert dataframe test to sparse matrix
  
  dtestset <- data.matrix(testdata)
  dtest <- xgb.DMatrix(dtestset)
  #在测试集上预测
  pred <- predict(xgb_model, dtest)
  # cat(pred, file = "/home/stucse/result.txt")
  return(pred)
}
#
#xgboost中的result还原为原始的字符串值
formatResult <- function(result, factoredLabel, sampleNames)
{
  result <- round(result, digits = 3)
  nclass <- length(levels(factoredLabel))
  idxVector <- c()
  predictorClass <- c()
  if( nclass > 2){
    nrow = length(result)/nclass
    result <- matrix(result, nrow = nrow,ncol = nclass, byrow = TRUE,  dimnames = list(sampleNames, levels(factoredLabel)))
    for (rowIdx in 1:nrow ){
      arow <- result[rowIdx,]
      idxVector <-c(idxVector, which.max(arow))
    }
    for (idx in idxVector) {
      predictorClass <- c(predictorClass, levels(factoredLabel)[idx])
    }
    result <- cbind(result, predictorClass)
  }else{
    result <- matrix(result, nrow = length(result),ncol = 1, byrow = TRUE, dimnames = list(sampleNames,c("prob") ))
    acol <- result[,1]
    for ( prob in acol) {
      idx <- 1
      if(prob > 0.5 ){
        idx <- 2
      }
      predictorClass <- c(predictorClass, levels(factoredLabel)[idx])
    }
    result <- cbind(result, predictorClass)
  }

  print(result)
  return( result)
}