BPR  <-  function(train_data,test_data,target)
{
  library(AMORE)
  # Vic_data <- category_to_numeric(train_data, test_data, type, out = F)
  # train_data <- Vic_data$train_data; test_data <- Vic_data$test_data
  ind <- which(colnames(train_data) == target)
  #BPÑµÁ·
  net <- newff(n.neurons=c(ncol(train_data),4,1), learning.rate.global=1e-2,
               momentum.global=0.5,error.criterium="LMS", 
               Stao=NA, hidden.layer="tansig", 
               output.layer="purelin", method="ADAPTgdwm")
  model <- train(net, train_data[,-ind], train_data[ind], 
                 error.criterium="LMS", 
                 report=TRUE, show.step=100, n.shows=5)
  train_pre <- as.vector(sim(model$net, train_data[,-ind]))
  test_pre <- as.vector(sim(model$net, test_data[,-ind]))
  
  test_sq <- sum((train_data[[ind]] - train_pre)^2)
  test_tot_sq <- sum((train_data[[ind]] - mean(train_data[[ind]]))^2)
  rsqua_test <- 1 - test_sq/test_tot_sq
  
  train_sq <- sum((test_data[[ind]] - test_pre)^2)
  train_tot_sq <- sum((test_data[[ind]] - mean(test_data[[ind]]))^2)
  rsqua_train <- 1 - train_sq/train_tot_sq
  
  library(pROC)
  ROC_plot <- function(train_pre,test_pre)
  {
    # ÑµÁ·¼¯ROC
    train_roc <- roc(response = train_data[ ,ind],predictor = train_pre,
                     smooth.method  = "density")
    # plot(train_roc,col='green',lty=2)
    # legend("bottomright",bty="n",legend = c(paste("AUC for train=",
    #                                               round(train_roc$auc,5))),
    #        col='green',lty=1,cex=0.9)
    auc_train <-  train_roc$auc
    # ²âÊÔ¼¯ROC
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

# BPR(train_data,test_data,target)