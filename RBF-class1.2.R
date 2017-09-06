RBF <- function(train_data,test_data,target,type)
{
  Vic_data <- category_to_numeric(train_data, test_data, type, out = F)
  train_data <- Vic_data$train_data; test_data <- Vic_data$test_data
  data <- rbind(train_data,test_data)
  
  library(RSNNS)
  ind <- which(colnames(data) == target)
  data[ ,-ind] <- sapply(data[ ,-ind], as.numeric)
  data[ ,ind] <- factor(data[,ind])
  data[,-ind] <- normalizeData(data[,-ind],"norm")
  
  
  #定位目标变量
  
  dataValues <- data[ ,-ind]
  dataTargets <- decodeClassLabels(data[ ,ind])
  data <- splitForTrainingAndTest(dataValues, dataTargets, ratio=0.3)
  data <- normTrainingAndTestSet(data)
  trainy <- encodeClassLabels(data$targetsTrain)
  testy <- encodeClassLabels(data$targetsTest)
  
  #利用mlp函数建立模型
  model <- mlp(data$inputsTrain, data$targetsTrain, size=5, learnFuncParams=c(0.1),
               maxit=50, inputsTest=data$inputsTest, targetsTest=data$targetsTest)
  
  #ROC
  library(pROC)
  train_pre <- predict(model,data$inputsTrain); train_pre <- encodeClassLabels(train_pre)
  test_pre <- predict(model,data$inputsTest); test_pre <- encodeClassLabels(test_pre)
  
  sum_test = 0
  for (i in 1:length(test_pre)){
    if(test_pre[i] == test_data[i,ind]){
      sum_test = sum_test + 1
    }
  }
  sum_train = 0
  for (i in 1:length(train_pre)){
    if(train_pre[i] == train_data[i,ind]){
      sum_train = sum_train + 1
    }
  }
  
  correct_test <- sum_test/length(test_pre)
  correct_train <- sum_train/length(train_pre)
  train_pre <- factor(train_pre,levels = levels(train_data[,ind]),ordered = T)
  test_pre <- factor(test_pre,levels = levels(test_data[,ind]),ordered = T)
  ROC_plot <- function(train_pre,test_pre)
  {
    # 训练集ROC
    train_roc <- roc(response = trainy,predictor = train_pre,
                     smooth.method  = "density")
    # plot(train_roc,col='green',lty=2)
    # legend("bottomright",bty="n",legend = c(paste("AUC for train=",
    #                                               round(train_roc$auc,5))),
    #        col='green',lty=1,cex=0.9)
    auc_train <-  train_roc$auc
    # 测试集ROC
    test_roc <- roc(response = testy,predictor = test_pre,
                    smooth.method  = "density")
    # plot(test_roc,col='green',lty=2)
    # legend("bottomright",bty="n",legend = c(paste("AUC for test=",
    #                                               round(test_roc$auc,5))),
    #        col='green',lty=1,cex=0.9)
    auc_test <-  test_roc$auc
    return(data.frame(auc_train,auc_test))
  }
  auc <- ROC_plot(train_pre,test_pre)
  
  return(list(auc=auc, correct_train=correct_train,
              correct_test=correct_test))
}
# RBF(train_data,test_data,target,type)
