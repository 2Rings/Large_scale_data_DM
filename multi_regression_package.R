logis <- function(train_data,test_data,target,type,target_order = F)
{
  # Vic_data <- category_to_numeric(train_data, test_data, type, out = F)
  # train_data <- Vic_data$train_data; test_data <- Vic_data$test_data
  ind <- which(colnames(train_data)==target)
  colnames(train_data)[ind] <- 'target'
  colnames(test_data)[ind] <- 'target'
  if (length(unique(train_data[,ind]))==2)
  {
    library(RSNNS)
    train_data[ ,-ind] <- sapply(train_data[ ,-ind], as.numeric)
    train_data[,-ind] <- normalizeData(train_data[,-ind],"norm")
    test_data[ ,-ind] <- sapply(test_data[ ,-ind], as.numeric)
    test_data[,-ind] <- normalizeData(test_data[,-ind],"norm")
    glm.fit=glm(target~., data=train_data, family=binomial(link="logit"))
    
    train_pre <- predict(glm.fit, train_data[,-ind], type = "response")
    train_pre <- 1*(train_pre>0.5)
    test_pre <- predict(glm.fit, test_data[,-ind], type = "response")
    test_pre <- 1*(test_pre>0.5)
    
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
    
    library(pROC)
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
    auc <- ROC_plot(train_pre, test_pre)
    
    return(list(auc=auc,correct_train=correct_train,
                correct_test=correct_test))
  }
  else
  {
    if (target_order == F)
    {
      library(nnet)
      library(MASS)
      multinom.fit <- multinom(target~., data = train_data)
      train_pre <- predict(multinom.fit, train_data[ ,-ind])
      test_pre <- predict(multinom.fit, test_data[ ,-ind])
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
      
      library(pROC)
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
      auc <- ROC_plot(train_pre, test_pre)
      
      return(list(auc=auc,correct_train=correct_train,
                  correct_test=correct_test))
    }
    else
    {
      library(MASS)
      polr<- polr(target~., data = train_data, method = c("probit")) 
      train_pre <- predict(polr, train_data[ ,-ind])
      test_pre <- predict(polr, test_data[ ,-ind])
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
      
      library(pROC)
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
      auc <- ROC_plot(train_pre, test_pre)
      
      return(list(auc=auc,correct_train=correct_train,
                  correct_test=correct_test))
    }
  }
}
# logis(train_data,test_data,target,type,target_order)

regres <- function(train_data,test_data,target)
{
  library(car)
  library(pROC)
  Vic_data <- category_to_numeric(train_data, test_data, type, out = F)
  train_data <- Vic_data$train_data; test_data <- Vic_data$test_data
  ind <- which(colnames(train_data)==target)
  colnames(train_data)[ind] <- 'target'
  colnames(test_data)[ind] <- 'target'
  
  train_data[] <- sapply(train_data, as.numeric)
  test_data[] <- sapply(test_data, as.numeric)
  regs <- lm(target~.,data=train_data)
  
  train_pre <- predict(regs, train_data[,-ind])
  test_pre <- predict(regs, test_data[,-ind])
  
  test_sq <- sum((train_data[[ind]] - train_pre)^2)
  test_tot_sq <- sum((train_data[[ind]] - mean(train_data[[ind]]))^2)
  rsqua_test <- 1 - test_sq/test_tot_sq
  
  train_sq <- sum((test_data[[ind]] - test_pre)^2)
  train_tot_sq <- sum((test_data[[ind]] - mean(test_data[[ind]]))^2)
  rsqua_train <- 1 - train_sq/train_tot_sq
  
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
  return(list(auc=auc,rsq_train=rsqua_train,rsq_test=rsqua_test))
}
# regres(train_data,test_data,target)


