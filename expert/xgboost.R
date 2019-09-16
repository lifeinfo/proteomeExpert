xgboost_classfier_training<-function(fileName){
  
  traindata <- read.csv(file = fileName, header = TRUE, quote = "", sep = ",")
  #print(dim(traindata))
  #print(class(traindata))
  #convert dataframe to sparse matrix and to list
  traindata1 <- data.matrix(traindata[,c(1:ncol(traindata)-1)]) 
  #print(dim(traindata1))
  # 利用Matrix函数，将sparse参数设置为TRUE，转化为稀疏矩阵
  traindata2 <- Matrix(traindata1,sparse=T) 
  #label is the last column
  traindata3 <- traindata[,ncol(traindata)]
  # 将自变量和因变量拼接为list
  traindata4 <- list(data=traindata2,label=traindata3) 
  # 构造模型需要的xgb.DMatrix对象，处理对象为稀疏矩阵
  
  dtrain <- xgb.DMatrix(data = traindata4$data, label = traindata4$label)
  #
  #xgboost参数
  #1,Booster,可选gblinear,gbtree,默认值为gbtree,
  #2,
  xgb_model <- xgboost(verbosity = 1,data = dtrain,max_depth=6, eta=0.5,  objective='binary:logistic', nround=20)
  
  return(xgb_model)
  
}
xgboost_classfier_predict <-function(xgb_model, test_data_file)
{
  test_data <- read.csv(file = test_data_file, header = TRUE, quote = "", sep = ",")
  test_data <- test_data[,c(1:ncol(test_data)-1)]
  #convert dataframe test to sparse matrix
  dtestset <- data.matrix(test_data)
  dtest <- xgb.DMatrix(dtestset)
  #在测试集上预测
  pred <- predict(xgb_model, dtest)
  return(pred)
}
