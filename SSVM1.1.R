SVM <- function(train_data,test_data,target)
{
  library(kernlab)
  Vic_data <- category_to_numeric(train_data, test_data, type, out = F)
  train_data <- Vic_data$train_data; test_data <- Vic_data$test_data
  ind <- which(colnames(train_data) == target)
  train_data[ ,-ind] <- sapply(train_data[ ,-ind], as.numeric)
  test_data[ ,-ind] <- sapply(test_data[ ,-ind], as.numeric)
  colnames(train_data)[ind]='target'
  colnames(test_data)[ind]='target'
  
  pre_scale_ind <- c()
  for (j in seq(ncol(train_data))[-ind])
  {
    if (length(unique(train_data[ ,j])) == 1)
    {
      pre_scale_ind <- c(pre_scale_ind, j)
    }
  }
  if (!is.null(pre_scale_ind))
  {
    train_data <- train_data[ ,-pre_scale_ind]
    test_data <- test_data[ ,-pre_scale_ind]
  }
  datamodel <- ksvm(target~., train_data,              
                    type = "C-bsvc", kernel = "rbfdot",                    
                    kpar = list(sigma = 0.1), C = 10,                    
                    prob.model = TRUE)
  
  #ROC
  library(pROC)
  ind <- which(colnames(train_data) == 'target')
  train_pre <- predict(datamodel,train_data[,-ind])
  test_pre <- predict(datamodel,test_data[,-ind])
  
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
    # ÑµÁ·¼¯ROC
    train_roc <- roc(response = train_data[,ind],predictor = train_pre,
                     smooth.method  = "density")
    # plot(train_roc,col='green',lty=2)
    # legend("bottomright",bty="n",legend = c(paste("AUC for train=",
    #                                               round(train_roc$auc,5))),
    #        col='green',lty=1,cex=0.9)
    auc_train <-  train_roc$auc
   
    # ²âÊÔ¼¯ROC
    
    test_roc <- roc(response = test_data[,ind],predictor = test_pre,
                    smooth.method  = "density")
    # plot(test_roc,col='green',lty=2)
    # legend("bottomright",bty="n",legend = c(paste("AUC for test=",
    #                                               round(test_roc$auc,5))),
    #        col='green',lty=1,cex=0.9)
    auc_test <-  test_roc$auc
    return(data.frame(auc_train,auc_test))
  }
  auc <- ROC_plot(train_pre,test_pre)
  
  return(list(auc=auc,correct_train=correct_train,
              correct_test=correct_test))
}

# SVM(train_data,test_data,target)





