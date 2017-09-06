SVMR <- function(train_data,test_data,target)
{
  library(rminer)
  Vic_data <- category_to_numeric(train_data, test_data, type, out = F)
  train_data <- Vic_data$train_data; test_data <- Vic_data$test_data
  #数据标准化 
  # train_data <- scale(train_data[,4:12]) #数据标准化 
  ind <- which(colnames(train_data) == target)
  colnames(train_data)[ind] <- 'target'
  colnames(test_data)[ind] <- 'target'

  svr<-fit(target~., train_data, model="svm") 
  
  #利用模型进行预测
  ind <- which(colnames(train_data) == target)
  train_pre <- predict(svr, train_data[,-ind]) 
  test_pre <- predict(svr, test_data[,-ind]) 
  
  #ROC
  library(pROC)
  test_sq <- sum((train_data[[ind]] - train_pre)^2)
  test_tot_sq <- sum((train_data[[ind]] - mean(train_data[[ind]]))^2)
  rsqua_test <- 1 - test_sq/test_tot_sq
  
  train_sq <- sum((test_data[[ind]] - test_pre)^2)
  train_tot_sq <- sum((test_data[[ind]] - mean(test_data[[ind]]))^2)
  rsqua_train <- 1 - train_sq/train_tot_sq
  
  ROC_plot <- function(train_pre,test_pre)
  {
    # 训练集ROC
    train_roc <- roc(response = train_data[ ,ind],predictor = train_pre,
                     smooth.method  = "density")
    # plot(train_roc,col='green',lty=2)
    # legend("bottomright",bty="n",legend = c(paste("AUC for train=",
    #                                               round(train_roc$auc,5))),
    #        col='green',lty=1,cex=0.9)
    auc_train <-  train_roc$auc
    # 测试集ROC
    test_roc <- roc(response = test_data[ ,ind],predictor = test_pre,
                    smooth.method  = "density")
    # plot(test_roc,col='green',lty=2)
    # legend("bottomright",bty="n",legend = c(paste("AUC for test=",
    #                                               round(test_roc$auc,5))),
    #        col='green',lty=1,cex=0.9)
    auc_test <-  test_roc$auc
    return(data.frame(auc_train,auc_test))
  }
  auc <- ROC_plot(train_pre,test_pre)
  
  return(list(auc=auc, rsq_train=rsqua_train, rsq_test=rsqua_test))
}
# SVMR(train_data,test_data,target)
