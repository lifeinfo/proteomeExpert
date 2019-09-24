xgboost_classfier_training<-function(trainX,trainY,parameters, numRounds){
  
  
  xg_train <- xgb.DMatrix(data = as.matrix(trainX), label = trainY)
  num <- length(unique(trainY))
  if(num != 2){
    parameters[["objective"]] <- 'multi:softmax'
    parameters[["num_class"]] <- num
  }
  bst <- xgb.train(
    params = params,
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
  return(pred)
}
