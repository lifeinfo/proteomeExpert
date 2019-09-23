xgboost_classfier_training<-function(trainData,parameters, numRounds){
  
  trainX <- trainData[,-1]
  #label is the first column
  trainY <- trainData[,1]
  
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
  # traindata1 <- data.matrix(traindata[,c(2:ncol(traindata))]) 
  # #print(dim(traindata1))
  # # 利用Matrix函数，将sparse参数设置为TRUE，转化为稀疏矩阵
  # traindata2 <- Matrix(traindata1,sparse=T) 
  # #label is the last column
  # #traindata3 <- traindata[,ncol(traindata)]
  # 
  # traindata3 <- traindata[,1]
  # traindata3 <- as.factor(traindata3)
  # obj <- 'binary:logistic'
  # if(length(unique(traindata3)) != 2 ){
  #   obj <- 'multi:softmax'
  # }
  # # 将自变量和因变量拼接为list
  # traindata4 <- list(data=traindata2,label=traindata3) 
  # # 构造模型需要的xgb.DMatrix对象，处理对象为稀疏矩阵
  # dtrain <- xgb.DMatrix(data = traindata4$data, label = traindata4$label)
  # #
  # #xgboost参数
  # #1,Booster,可选gblinear,gbtree,默认值为gbtree,
  # #2,
  # xgb_model <- xgboost(verbosity = 1,data = dtrain,max_depth=6, eta=0.5,  objective='binary:logistic', nround=20)
  # 
  # return(xgb_model)
  
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
