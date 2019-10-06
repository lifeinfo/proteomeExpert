xgboost_classfier_training<-function(trainX,trainY,parameters, numRounds){
  
  num <- length(levels(trainY))
  if(num != 2){
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
xgboost_classfier_predict <-function(xgb_model, test_data_file)
{
  test_data <- read.csv(file = test_data_file, header = TRUE, quote = "", sep = ",")
  #there is no label in the test_data
  
  #test_data <- test_data[,c(1:ncol(test_data)-1)]
  #convert dataframe test to sparse matrix
  dtestset <- data.matrix(test_data)
  dtest <- xgb.DMatrix(dtestset)
  #在测试集上预测
  pred <- predict(xgb_model, dtest)
  # cat(pred, file = "/home/stucse/result.txt")
  return(pred)
}
