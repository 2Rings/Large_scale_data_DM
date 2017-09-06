#---------------------------------------------------------------------
#参数接口：数据集DATA，数据的类型type，目标变量target
#type是一个和DATA列数相等的数值向量，取值为（1,0），1表示离散型变量，
#0表示连续型变量；target为目标变量列的名字
#---------------------------------------------------------------------
# load("E:/学习/应用统计/Rwspace/ab.Rdata")
# DATA <- ab;type=c(1,0,0,0,0,0,0,0,0);target='rings'
DT <- function(train_data, test_data, type, target)
{
  #--------------------------
  #安装并载入需要的包
  #--------------------------
  # install.packages('randomForest')
  # install.packages('C50')
  # install.packages('rpart')
  # install.packages('pROC')
  # install.packages('adabag')
  # install.packages('gbm')
  # install.packages('varSelRF')
  # install.packages("VSURF")
  library(randomForest)
  library(C50)
  library(rpart)
  library(pROC)
  library(adabag)
  library(gbm)
  library(VSURF)
  #--------------------------
  #将特征变量中的
  #--------------------------
  dat <- function(train_data, test_data, type, target)
  {
    ind <- which(colnames(train_data) == target)
    if(type[ind] == 1)
    {
      train_data[,ind] <- as.factor(train_data[,ind])
      test_data[,ind] <- as.factor(test_data[,ind])
      levels(test_data[,ind]) <- levels(train_data[,ind])
    } else
    {
      train_data[ ,ind] <- as.numeric(train_data[ ,ind])
      test_data[ ,ind] <- as.numeric(test_data[ ,ind])
    }
    for (i in (1:length(type))[-ind])
    {
      if(type[i]==0)
      {
        train_data[ ,i] <- as.numeric(train_data[,i])
        test_data[ ,i] <- as.numeric(test_data[ ,i])
      } 
      else 
      {
        train_data[,i] <- as.factor(train_data[,i])
        test_data[,i] <- as.factor(test_data[,i])
        levels(test_data[,i]) <- levels(train_data[,i])
      }
    }
    return(list(train_data = train_data, test_data = test_data))
  }
  
  #---------------------------------------------------------------------
  #基于随机森林
  #---------------------------------------------------------------------
  RF <- function(train, test, type, target)
  {
    ind <- which(colnames(train) == target)
    del_lev <- c() 
    for (j in (1:length(type))[-ind])
    {
      if (type[j] != 0)
      {
        lev_length <- length(levels(train[ ,j]))
        if (lev_length >= 53)
        {
          del_lev <- c(del_lev, j)
        }
      }
    }
    if (length(del_lev) != 0)
    {
      train <- train[ ,-del_lev]
      test <- test[ ,-del_lev]
      type <- type[-del_lev]
      ind <- which(colnames(train) == target)
    }
    
    trainx <- train[ ,-ind]; trainy <- train[ ,ind]
    testx <- test[ ,-ind]; testy <- test[ ,ind]
    
    # #筛选变量
    # if (length(train) >= 30)
    # {
    #   if(type[ind] == 1)
    #   {
    #     rf_sel <- VSURF(trainx, trainy, ntree = 500, parallel = TRUE)
    #     
    #     imp <- rf_sel$selected.vars
    #     # plot(rf_sel)
    #     trainx <- trainx[ ,imp]
    #     testx <- testx[ ,imp]
    #   }  else
    #   {
    #     rf_sel <- VSURF(trainx, trainy, ntree = 500, parallel = TRUE)
    #     trainx <- trainx[ ,imp]
    #     testx <- testx[ ,imp]
    #   }
    # }
    
    rf <- randomForest(trainx, trainy, ntree = 500, 
                       mtry = log2(ncol(trainx)),
                       importance=T, proximity=TRUE)
    task <- rf$type
    # plot(rf,main=deparse(substitute(x)))
    
    
    #AUC
    #预测训练集;预测测试集
    train_pre <- predict(rf, newdata = trainx)
    test_pre <- predict(rf, newdata = testx)
    
    
    if (task == "classification")
    {
      train_pre <- factor(train_pre,levels = levels(trainy),ordered = T)
      test_pre <- factor(test_pre,levels = levels(testy),ordered = T)
      
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
    } else
    {
      test_sq <- sum((train_data[[ind]] - train_pre)^2)
      test_tot_sq <- sum((train_data[[ind]] - mean(train_data[[ind]]))^2)
      rsqua_test <- 1 - test_sq/test_tot_sq
      
      train_sq <- sum((test_data[[ind]] - test_pre)^2)
      train_tot_sq <- sum((test_data[[ind]] - mean(test_data[[ind]]))^2)
      rsqua_train <- 1 - train_sq/train_tot_sq
    }
    
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
    
    if (task == "classification")
    {
      return(list(auc=auc,correct_train=correct_train,
                  correct_test=correct_test))
    } else
    {
      return(list(auc=auc,rsq_train=rsqua_train,rsq_test=rsqua_test))
    }
  }
  
  #---------------------------------------------------------------------
  #基于C5.0
  #---------------------------------------------------------------------
  C5 <- function(train,test)
  {
    ind <- which(colnames(train)==target)
    trainx<-train[,-ind];trainy<-train[,ind]
    testx<-test[,-ind]; testy<-test[,ind] 
    
    c5.0 <- C5.0(trainx,trainy,control = C5.0Control(winnow = TRUE))
    # plot(c5.0)
    # imp <- C5imp(c5.0,metric = 'usage')
    
    #AUC
    #预测训练集;预测测试集
    train_pre <- predict(c5.0, newdata = trainx)
    test_pre <- predict(c5.0, newdata = testx)
    
    train_pre <- factor(train_pre,levels = levels(trainy),ordered = T)
    test_pre <- factor(test_pre,levels = levels(testy),ordered = T)
    
    # 预测准确率
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
    return(list(auc=auc,correct_train=correct_train,
                correct_test=correct_test))
  }
  
  #---------------------------------------------------------------------
  #基于CART
  #---------------------------------------------------------------------
  CART <- function(train,test)
  {
    ind <- which(colnames(train)==target)
    trainx<-train[,-ind];trainy<-train[,ind]
    testx<-test[,-ind]; testy<-test[,ind] 
    colnames(train)[ind] <- 'target'
    colnames(test)[ind] <- 'target'
    
    task <- class(train$target)
    if(task=='factor')
    {
      method <- 'class'
    } 
    else 
    {
      method <- 'anova'
    }
    fit <- rpart(target~.,data=train,method=method,
                 control = rpart.control(xval = 10,cp = 0.02))
    # par(xpd = NA)
    # plot(fit)
    # text(fit, use.n = TRUE,cex = 0.8)
    # printcp(fit)#输出剪枝表格
    
    # imp <- fit$variable.importance
    #AUC
    #预测训练集;预测测试集
    if(task=='factor')
    {
      train_pre <- predict(fit, newdata = trainx,type = 'class')
      test_pre <- predict(fit, newdata = testx,type = 'class')
      train_pre <- factor(train_pre,levels = levels(trainy),ordered = T)
      test_pre <- factor(test_pre,levels = levels(testy),ordered = T)
      
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
    }  else
    {
      train_pre <- predict(fit, newdata = trainx,type = 'matrix')
      test_pre <- predict(fit, newdata = testx,type = 'matrix')
      
      test_sq <- sum((train_data[[ind]] - train_pre)^2)
      test_tot_sq <- sum((train_data[[ind]] - mean(train_data[[ind]]))^2)
      rsqua_test <- 1 - test_sq/test_tot_sq
      
      train_sq <- sum((test_data[[ind]] - test_pre)^2)
      train_tot_sq <- sum((test_data[[ind]] - mean(test_data[[ind]]))^2)
      rsqua_train <- 1 - train_sq/train_tot_sq
    }
    
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
    if (task == 'factor')
    {
      return(list(auc=auc,correct_train=correct_train,
                  correct_test=correct_test))
    } else
    {
      return(list(auc=auc,rsq_train=rsqua_train,rsq_test=rsqua_test))
    }
  }
  
  #---------------------------------------------------------------------
  #基于adaboost
  #---------------------------------------------------------------------
  adaboost <- function(train,test)
  {
    ind <- which(colnames(train)==target)
    trainx<-train[,-ind];trainy<-train[,ind]
    testx<-test[,-ind]; testy<-test[,ind] 
    colnames(train)[ind] <- 'target'
    colnames(test)[ind] <- 'target'
    
    ada <- boosting(target~.,data=train,boos = T,mfinal = 80) 
    # ada$class
    # imp <- ada$importance
    #AUC
    #预测训练集;预测测试集
    train_pre <- predict(ada, newdata = trainx);train_pre <- train_pre$class
    test_pre <- predict(ada, newdata = testx);test_pre <- test_pre$class
    
    train_pre <- factor(train_pre,levels = levels(trainy),ordered = T)
    test_pre <- factor(test_pre,levels = levels(testy),ordered = T)
    
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
    return(list(auc=auc,correct_train=correct_train,
                correct_test=correct_test))
  }
  
  #---------------------------------------------------------------------
  #基于gbdt
  #---------------------------------------------------------------------
  gbdt <- function(train,test)
  {
    ind <- which(colnames(train)==target)
    trainx<-train[,-ind];trainy<-train[,ind]
    testx<-test[,-ind]; testy<-test[,ind] 
    colnames(train)[ind] <- 'target'
    colnames(test)[ind] <- 'target'
    
    gbt <- gbm(target~.,data=train,distribution="gaussian",      
               n.trees=393,shrinkage=0.01,interaction.depth=3,          
               bag.fraction = 0.5,train.fraction = 0.5,cv.folds = 10,                   
               keep.data=TRUE,verbose='cv',n.cores=2)   
    best.iter <- gbm.perf(gbt,method="cv",plot.it = F)
    # print(best.iter)
    # imp <- summary(gbt,n.trees=best.iter,plotit = F)
    #AUC
    #预测训练集;预测测试集
    train_pre <- predict(gbt, newdata = trainx)
    test_pre <- predict(gbt, newdata = testx)
    
    test_sq <- sum((train_data[[ind]] - train_pre)^2)
    test_tot_sq <- sum((train_data[[ind]] - mean(train_data[[ind]]))^2)
    rsqua_test <- 1 - test_sq/test_tot_sq
    
    train_sq <- sum((test_data[[ind]] - test_pre)^2)
    train_tot_sq <- sum((test_data[[ind]] - mean(test_data[[ind]]))^2)
    rsqua_train <- 1 - train_sq/train_tot_sq
    
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
    return(list(auc=auc,rsq_train=rsqua_train,rsq_test=rsqua_test))
  }
  
  #——————————————————————————————————————————————————————
  #打包算法
  #——————————————————————————————————————————————————————
  DT_pack <- function(train,test,k,n)
  {
    ind <- which(colnames(train)==target)
    if(type[ind]==1)
    {
      if(k<10|n<50)
      {
        p1 <- runif(1)
        if(p1<0.6)
        {
          C5(train,test)
        }
        else
        {
          CART(train,test)
        }
      } 
      else if((k<30&k>=10)|(50<n&n<150))
      {
        p1 <- runif(1)
        if(p1<0.6)
        {
          CART(train,test)
        }
        else
        {
          p2 <- runif(1)
          if(p2<-0.6)
          {
            adaboost(train,test)
          } 
          else {RF(train,test)}
        }
      }
      else if(k>30&n>150)
      {
        p1 <- runif(1)
        if(p1<-0.6)
        {
          adaboost(train,test)
        } 
        else {RF(train,test)}
      }
    }
    else
    {
      if(k<10|n<50)
      {
        CART(train,test)
      }
      else if((k<30&k>10)|(50<n&n<150))
      {
        p1 <- runif(1)
        if(p1<0.6)
        {
          CART(train,test)
        }
        else
        {
          p2 <- runif(1)
          if(p2<-0.6)
          {
            gbdt(train,test)
          }
          else {RF(train,test)}
        }
      }
      else if(k>30&n>150)
      {
        p1 <- runif(1)
        if(p1<-0.6)
        {
          gbdt(train,test)
        }
        else {RF(train,test)}
      }
    }
  }
  
  data_resu <- dat(train_data, test_data, type, target)
  train <- data_resu$train_data
  test <- data_resu$test_data
  DT_pack(train,test,k=ncol(train)-1,n=nrow(train))
}


