# 处理缺失值----------------------------------------------------------------
# 需要提供的参数：DATA，type，express,target
# 指定缺失值所占比例达到ratio时被删除
# 指定缺失值的表示express;out参数表示是否以文件的形式输出
# ------------------------------------------------------------------------
data_preprocess <- function(data,type,express = NULL,target,out = T)
{
  if (!is.null(express))
  {
    express <- append(express,NA)
  }
  # 去掉数据中两端的空格-------------------------------------
  space_del <- function(data, type)
  {
    for(j in seq(ncol(data)))
    {
      if (type[j] == 0)
      {
        tmp <- gsub('^ *| *$', '', data[ ,j])
        data[ ,j] <- as.numeric(tmp)
      }
      else
      {
        tmp <- gsub('^ *| *$', '', data[ ,j])
        data[ ,j] <- as.character(tmp)
      }
    }
    data
  }
  
  # 删除缺失达到一定比例的行列----------------------------
  del_na_reach_ratio <- function (data,type,express,ratio = 0.5)
  {
    if (!is.null(express))
    {
      delet_col <- c()
      for (j in 1:ncol(data))
      {
        for(i in express)
        {
          if(is.na(i))
          {
            if(anyNA(data[i,]))
            {
              na_ind <- is.na(data[,j])
              if((sum(na_ind)/length(data[,j]))>ratio)
              {
                delet_col <- c(delet_col,j)
                break
              }
            }
          }
          else
          {
            if(i %in% data[,j])
            {
              na_num <- length(which(data[,j]==i))
              if((na_num/length(data[,j]))>ratio)
              {
                delet_col <- c(delet_col,j)
                break
              }
            }
          }
        }
      }
      if (!is.null(delet_col))
      {
        data <- data[,-delet_col]
        type <- type[-delet_col]
      }
      
      delet_row <- c()
      for (i in 1:nrow(data)) 
      {
        for(l in express)
        {
          if(is.na(l))
          {
            if(anyNA(data[i,]))
            {
              na_num <- is.na(data[i,])
              if((sum(na_num)/length(data[i,]))>ratio)
              {
                delet_row <- c(delet_row,i)
                break
              }
            }
          }
          else
          {
            if(l %in% data[i,])
            {
              na_num <- length(which(data[i,]==l))
              if((na_num/length(data[i,]))>ratio)
              {
                delet_row <- c(delet_row,i)
                break
              }
            }
          }
        }
      }
      
      if (!is.null(delet_row))
      {
        data <- data[-delet_row, ]
      }
    }
    
    return(list(data = data,type = type))
  }
  
  # 寻找数据某一列缺失值的索引-------------------
  find_na_ind <- function (data, express, indx)
  {
    variable_na_ind <- c()
    for (i in express)
    {
      if (is.na(i))
      {
        if (anyNA(data[ ,indx]))
        {
          tmp_ind <- which(is.na(data[ ,indx]))
          variable_na_ind <- c(variable_na_ind,tmp_ind)
        }
      }
      else
      {
        if (i %in% data[ ,indx])
        {
          tmp_ind <- which(data[ ,indx] == i)
          variable_na_ind <- c(variable_na_ind,tmp_ind)
        }
      }
    }
    return(variable_na_ind)
  }
  
  # 填补缺失值---------------------------------------------
  fill_Missing <-function(data,type,express,target)
  {
    if (!is.null(express))
    {
      ind <- which(colnames(data)==target)
      
      if(type[ind] == 1)
      {
        # 填补目标变量中的缺失值---------------------
        target_na_ind <- find_na_ind(data,express,indx = ind)
        if (!is.null(target_na_ind))
        {
          target_not_na_ind <- (1:nrow(data))[-target_na_ind]
          
          category_table <- table(as.character(data[target_not_na_ind,ind]))
          category_mode_ind <- which.max(category_table)
          category_mode <- names(category_table)[category_mode_ind]
          
          data[target_na_ind,ind] <- category_mode
        }
        
        
        # 填补自变量中的缺失值---------------------
        for (j in (1:ncol(data))[-ind])
        {
          l_data <- data[,ind]
          if (type[j] == 1)
          {
            feature_na_ind <- find_na_ind(data,express,indx = j)
            if (!is.null(feature_na_ind))
            {
              feature_not_na_ind <- (1:nrow(data))[-feature_na_ind]
              
              for (l in unique(data[ ,ind]))
              {
                category_ind <- intersect(which(l_data == l),feature_not_na_ind)
                l1 <- length(category_ind)
                tmp_ind <- intersect(which(l_data == l),feature_na_ind)
                l2 <- length(tmp_ind)
                if (l2 != 0)
                {
                  if (l2 / (l2+l1) > 0.5 | l1 == 0)
                  {
                    u <- unique(data[ ,j])
                    data[tmp_ind,j] <- as.character(length(u)+1)
                  }
                  else
                  {
                    feature_table <- table(data[category_ind,j])
                    feature_mode_ind <- which.max(feature_table)
                    feature_mode <- names(feature_table)[feature_mode_ind]
                    data[tmp_ind,j] <- feature_mode
                  }
                }
              }
            }
          }
          else if (type[j] == 0)
          {
            feature_na_ind <- find_na_ind(data,express,indx = j)
            if (!is.null(feature_na_ind))
            {
              feature_not_na_ind <- (1:nrow(data))[-feature_na_ind]
              
              for (l in unique(data[ ,ind]))
              {
                category_ind <- intersect(which(l_data == l),feature_not_na_ind)
                l1 <- length(category_ind)
                tmp_ind <- intersect(which(l_data == l),feature_na_ind)
                l2 <- length(tmp_ind)
                
                if (l2 != 0)
                {
                  if (l2 / (l2+l1) > 0.5 | l1 == 0)
                  {
                    min_feature <- min(data[feature_not_na_ind,j])
                    max_feature <- max(data[feature_not_na_ind,j])
                    data[tmp_ind,j] <- runif(n = l2,min = min_feature,
                                             max = max_feature)
                  }
                  else
                  {
                    feature_median <- median(data[category_ind,j])
                    data[tmp_ind,j] <- feature_median
                  }
                }
              }
            }
          }
        }
      }
      if(type[ind] == 0)
      {
        # 填补目标变量中的缺失值---------------------
        target_na_ind <- find_na_ind(data,express,indx = ind)
        if (!is.null(target_na_ind))
        {
          target_not_na_ind <- (1:nrow(data))[-target_na_ind]
          
          if (!is.null(target_not_na_ind))
          {
            tmp <- as.character(data[target_not_na_ind,ind])
            target_median <- median(as.numeric(tmp))
            data[target_na_ind,ind] <- target_median
          }
        }
        
        # 填补自变量中的缺失值---------------------
        for (j in (1:ncol(data))[-ind])
        {
          if (type[j] == 1)
          {
            feature_na_ind <- find_na_ind(data,express,indx = j)
            if (!is.null(feature_na_ind))
            {
              feature_not_na_ind <- (1:nrow(data))[-feature_na_ind]
              
              feature_table <- table(data[feature_not_na_ind,j])
              feature_mode_ind <- which.max(feature_table)
              feature_mode <- names(feature_table)[feature_mode_ind]
              
              data[feature_na_ind,j] <- feature_mode
            }
          }
          else if (type[j] == 0)
          {
            feature_na_ind <- find_na_ind(data,express,indx = j)
            if (!is.null(feature_na_ind))
            {
              feature_not_na_ind <- (1:nrow(data))[-feature_na_ind]
              tmp <- as.character(data[feature_not_na_ind,j])
              feature_median <- median(as.numeric(tmp))
              data[feature_na_ind,j] <- feature_median
            }
          }
        }
      }
    }
    
    data
  }
  
  # 填补测试集----------------------------------------------
  test_fill_missing <- function(test_data, train_data, express, target, type)
  {
    test_name <- colnames(train_data)
    test_data <- test_data[ ,test_name]
    ind <- which(colnames(test_data) == target)
    
    target_na_ind <- find_na_ind(test_data, express, ind)
    if (!is.null(target_na_ind))
    {
      test_data <- test_data[-target_na_ind, ]
    }
    
    if (!is.null(express))
    {
      for (j in seq(ncol(test_data))[-ind])
      {
        feature_na_ind <- find_na_ind(test_data, express, j)
        test_data[feature_na_ind,j] <- sample(x = train_data[[j]],
                                              size = length(feature_na_ind),
                                              replace = T)
      }
    }
    
    for (j in (1:ncol(train_data))[-ind])
    {
      if (type[j] != 0)
      {
        train_feature_uniq <- unique(train_data[ ,j])
        test_feature_uniq <- unique(test_data[ ,j])
        dif <- setdiff(test_feature_uniq, train_feature_uniq)
        if (length(dif) != 0)
        {
          express_dif <- sapply(dif, list)
          ind_dif <- find_na_ind(test_data, express = express_dif, indx = j)
          test_data[ind_dif,j] <- sample(x = train_data[[j]],
                                         size = length(ind_dif),
                                         replace = T)
        }
      }
    }
    test_data
  }
  
  # 函数实现,并将数据分成训练集和测试集--------------------------
  data <- space_del(data, type)
  n <- dim(data)[1]; p <- 0.7
  index <- sample(1:n, n*p)
  train_data <- data[index, ]; test_data <- data[-index, ]
  
  train_data_type <- del_na_reach_ratio(train_data, type, express, ratio = 0.5)
  train_data <- train_data_type$data; type <- train_data_type$type
  
  train_data <- fill_Missing(train_data, type, express, target)
  test_data <- test_fill_missing(test_data, train_data,express,target,type)
  
  if (out)
  {
    # 输出数据------------------------------------------------
    save(train_data,file = 'train_data.RData')
    save(test_data,file = 'test_data.RData')
    save(type,file = 'type.RData')
    save(target,file = 'target.RData')
  }
  else 
  {
    return(list(train_data = train_data, test_data = test_data,
                type = type, target = target))
  }
}
#data_preprocess(data,type,express = NULL,target,out = T)

# 将名义型变量数值化-------------------------------------------------------
category_to_numeric <- function(train_data, test_data,target,type, out = T)
{
  raw_var_train <- list()
  raw_var_test <- list()
  count <- 0
  for (i in seq(ncol(train_data)))
  {
    if (type[i] != 0)
    {
      count <- count + 1
      raw_tmp_train <- unique(as.character(train_data[[i]]))
      raw_tmp_test <- unique(as.character(test_data[[i]]))
      
      raw_train_numeric <- seq(length(raw_tmp_train))-1
      name <- colnames(train_data)[i]
      # 保留训练集名义型变量转换为数值的对应关系---------------
      raw_var_train[[count]] <- list(raw_train = raw_tmp_train, 
                                     raw_train_numeric = raw_train_numeric, 
                                     name = name)
      
      tmp_train <- train_data[ ,i]; tmp_test <- test_data[ ,i]
      indx <- c()
      for (l in seq(length(raw_tmp_train)))
      {
        repl_ind_train <- which(tmp_train == raw_tmp_train[l])
        if (length(repl_ind_train) != 0)
        {
          tmp_train[repl_ind_train] <- as.character(raw_train_numeric[l])
        }
        
        repl_ind_test <- which(tmp_test == raw_tmp_train[l])
        if (length(repl_ind_test) !=0 )
        {
          tmp_test[repl_ind_test] <- as.character(raw_train_numeric[l])
          indx <- c(indx, l)
        }
      }
      train_data[ ,i] <- as.numeric(tmp_train)
      # 保留测试集名义型变量转换为数值的对应关系---------------
      raw_test <- raw_tmp_train[indx]
      raw_test_numeric <- raw_train_numeric[indx]
      
      dif <- setdiff(raw_tmp_test,raw_tmp_train)
      if (length(dif) != 0)
      {
        res <- (length(raw_train_numeric)+1):(length(raw_train_numeric)+
                                                length(dif)) - 1
        raw_test <- c(raw_tmp_test, dif)
        raw_test_numeric <- c(raw_test_numeric, res)
        for (j in 1:length(dif))
        {
          repl_ind_test <- which(tmp_test == dif[j])
          tmp_test[repl_ind_test] <- as.character(res[j])
        }
        
      }
      test_data[ ,i] <- as.numeric(tmp_test)
      raw_var_test[[count]] <- list(raw_test = raw_test, 
                                    raw_test_numeric = raw_test_numeric,
                                    name = name)
      
    }
  }
  ind <- which(colnames(train_data) == target)
  for(j in seq(ncol(train_data))[-ind])
  {
    train_data[ ,j] <- as.numeric(train_data[[j]])
    test_data[ ,j] <- as.numeric(test_data[[j]])
    
  }
  if (type[ind] == 1)
  {
    train_data[ ,ind] <- as.factor(train_data[[ind]])
    test_data[ ,ind] <- as.factor(test_data[[ind]])
    levels(test_data[ ,ind]) <- levels(train_data[[ind]])
  }
  if (out)
  {
    # 输出数据------------------------------------------------
    save(train_data, file = 'train_data.RData')
    save(test_data, file = 'test_data.RData')
    save(type, file = 'type.RData')
    save(target, file = 'target.RData')
  }
  else 
  {
    return(list(train_data = train_data, test_data = test_data,
                type = type, target = target))
  }
} 
#category_to_numeric(train_data, test_data,target,type, out = T)

# logistic分类------------------------------------------------------
logis <- function(train_data,test_data,target,type,target_order = F)
{
  Vic_data <- category_to_numeric(train_data, test_data,target,type, out = F)
  train_data <- Vic_data$train_data; test_data <- Vic_data$test_data
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
      # 训练集ROC
      train_roc <- roc(response = train_data[,ind],predictor = train_pre,
                       smooth.method  = "density")
      # plot(train_roc,col='green',lty=2)
      # legend("bottomright",bty="n",legend = c(paste("AUC for train=",
      #                                               round(train_roc$auc,5))),
      #        col='green',lty=1,cex=0.9)
      auc_train <-  train_roc$auc
      
      # 测试集ROC
      
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
        # 训练集ROC
        train_roc <- roc(response = train_data[,ind],predictor = train_pre,
                         smooth.method  = "density")
        # plot(train_roc,col='green',lty=2)
        # legend("bottomright",bty="n",legend = c(paste("AUC for train=",
        #                                               round(train_roc$auc,5))),
        #        col='green',lty=1,cex=0.9)
        auc_train <-  train_roc$auc
        
        # 测试集ROC
        
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
        # 训练集ROC
        train_roc <- roc(response = train_data[,ind],predictor = train_pre,
                         smooth.method  = "density")
        # plot(train_roc,col='green',lty=2)
        # legend("bottomright",bty="n",legend = c(paste("AUC for train=",
        #                                               round(train_roc$auc,5))),
        #        col='green',lty=1,cex=0.9)
        auc_train <-  train_roc$auc
        
        # 测试集ROC
        
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
#logis(train_data,test_data,target,type,target_order)

# 多元回归-------------------------------------
regres <- function(train_data,test_data,target)
{
  library(car)
  library(pROC)
  Vic_data <- category_to_numeric(train_data, test_data,target,type, out = F)
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
  return(list(auc=auc))
}
#regres(train_data,test_data,target)

# SVM回归------------------------------------
SVMR <- function(train_data,test_data,target)
{
  Vic_data <- category_to_numeric(train_data, test_data,target,type, out = F)
  train_data <- Vic_data$train_data; test_data <- Vic_data$test_data
  ind <- which(colnames(train_data) == target)
  colnames(train_data)[ind] <- 'target'
  colnames(test_data)[ind] <- 'target'
  
  library(rminer)
  svr<-fit(target~., train_data, model="svm") 
  
  #利用模型进行预测 
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
  
  return(list(auc=auc))
}
#SVMR(train_data,test_data,target)

# SVM分类------------------------------------
SVM <- function(train_data,test_data,target)
{
  library(kernlab)
  Vic_data <- category_to_numeric(train_data, test_data,target,type, out = F)
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
  ind <- which(colnames(train_data) == "target")
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
    # 训练集ROC
    train_roc <- roc(response = train_data[,ind],predictor = train_pre,
                     smooth.method  = "density")
    # plot(train_roc,col='green',lty=2)
    # legend("bottomright",bty="n",legend = c(paste("AUC for train=",
    #                                               round(train_roc$auc,5))),
    #        col='green',lty=1,cex=0.9)
    auc_train <-  train_roc$auc
    
    # 测试集ROC
    
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
SVM(train_data,test_data,target)

# 决策树--------------------------------------------
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
          if(p2<0.6)
          {
            adaboost(train,test)
          } 
          else {RF(train,test)}
        }
      }
      else if(k>30&n>150)
      {
        p1 <- runif(1)
        if(p1<0.6)
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
          if(p2<0.6)
          {
            gbdt(train,test)
          }
          else {RF(train,test)}
        }
      }
      else if(k>30&n>150)
      {
        p1 <- runif(1)
        if(p1<0.6)
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
#DT(train_data, test_data, type, target)

# BP回归--------------------------------------
BPR  <-  function(train_data,test_data,target)
{
  library(AMORE)
  Vic_data <- category_to_numeric(train_data, test_data,target,type, out = F)
  train_data <- Vic_data$train_data; test_data <- Vic_data$test_data
  ind <- which(colnames(train_data) == target)
  #BP训练
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
  
  return(list(auc=auc))
}  
#BPR(train_data,test_data,target)

# BP分类-----------------------------------------
RBF <- function(train_data,test_data,target,type)
{
  Vic_data <- category_to_numeric(train_data, test_data,target,type, out = F)
  train_data <- Vic_data$train_data; test_data <- Vic_data$test_data
  data <- rbind(train_data,test_data)
  
  library(RSNNS)
  ind <- which(colnames(data) == target)
  data[ ,-ind] <- sapply(data[ ,-ind], as.numeric)
  data[ ,ind] <- factor(data[,ind])
  data[,-ind] <- normalizeData(data[,-ind],"norm")
  
  
  #定位目标变量
  
  dataValues <- data[ ,-ind]
  # dataTargets <- decodeClassLabels(data[ ,ind])
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
    if(test_pre[i] == testy[i]){
      sum_test = sum_test + 1
    }
  }
  sum_train = 0
  for (i in 1:length(train_pre)){
    if(train_pre[i] == trainy[i]){
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
#RBF(train_data,test_data,target,type)

# 主程序----------------------------------------------------------------
DM <- function(data, type, express = NULL , target,target_order = F)
{
  data_ <- data_preprocess(data, type, express, target, out = F)
  train_data <- data_$train_data; test_data <- data_$test_data
  type <- data_$type; target <- data_$target; rm(data)

  ind <- which(colnames(train_data) == target)
  if (type[ind] == 1)
  {
    dim_ <- dim(data)[2] - 1
    if (dim_ < 4)
    {
      cat('Run model:','logistic','\n')
      logis_model <-logis(train_data,test_data,target,type,target_order)
      logis_auc <- logis_model$auc[2]
      if (logis_auc >= 0.7)
      {
        cat('use model:','logistic','\n')
        return(logis_model)
      }
      else
      {
        cat('Run model:','SVM','\n')
        svm_model <- SVM(train_data,test_data,target)
        svm_auc <- svm_model$auc[2]
        if(svm_auc >= 0.7)
        {
          cat('use model:','SVM','\n')
          return(svm_model)
        }
        else
        {
          cat('Run model:','tree','\n')
          tree_model <- DT(train_data, test_data, type, target)
          tree_auc <- tree_model$auc[2]
          if (tree_auc >= 0.7)
          {
            cat('use model:','tree','\n')
            return(tree_model)
          }
          else
          {
            cat('Run model:','RBF','\n')
            bp_model <- RBF(train_data,test_data,target,type)
            bp_auc<-bp_model$auc[2]
            if(bp_auc >= 0.7)
            {
              cat('use model:','RBF','\n')
              return(bp_model)
            }
            else
            {
              result_list <- list(logistic=logis_model,SVM=svm_model,tree=tree_model,BP=bp_model)
              auc_ <- c(logis_auc,svm_auc,tree_auc,bp_auc)
              name <- c('logistic','SVM','tree','BP')
              max = which.max(auc_)
              cat('best model:',name[max],'\n')
              return(result_list[[max]])
            }
          }
        }
      }
    } else
    {
      cat('Run model:','SVM','\n')
      svm_model <- SVM(train_data,test_data,target)
      svm_auc <- svm_model$auc[2]
      if(svm_auc >= 0.7)
      {
        cat('use model:','SVM','\n')
        return(svm_model)
      }
      else
      {
        cat('Run model:','tree','\n')
        tree_model <- DT(train_data, test_data, type, target)
        tree_auc <- tree_model$auc[2]
        if (tree_auc >= 0.7)
        {
          cat('use model:','tree','\n')
          return(tree_model)
        }
        else
        {
          cat('Run model:','RBF','\n')
          bp_model <- RBF(train_data,test_data,target,type)
          bp_auc <- bp_model$auc[2]
          if(bp_auc >= 0.7)
          {
            cat('use model:','RBF','\n')
            return(bp_model)
          }
          else
          {
            result_list <- list(SVM=svm_model,tree=tree_model,BP=bp_model)
            auc_ <- c(svm_auc,tree_auc,bp_auc)
            name <- c('SVM','tree','BP')
            max = which.max(auc_)
            cat('best model:',name[max],'\n')
            return(result_list[[max]])
          }
        }
      }
    }
  }  else
  {
    cat('Run model:','regression','\n')
    regres_model <- regres(train_data,test_data,target)
    regre_auc <- regres_model$auc[2]
    if (regre_auc >= 0.7)
    {
      cat('use model:','regression','\n')
      return(regres_model)
    }  else
    {
      cat('Run model:','SVM','\n')
      svm_model <- SVMR(train_data,test_data,target)
      svm_auc <- svm_model$auc[2]
      if(svm_auc >= 0.7)
      {
        cat('use model:','SVM','\n')
        return(svm_model)
      }
      else
      {
        cat('Run model:','tree','\n')
        tree_model <- DT(train_data, test_data, type, target)
        tree_auc <- tree_model$auc[2]
        if (tree_auc >= 0.7)
        {
          cat('use model:','tree','\n')
          return(tree_model)
        }
        else
        {
          cat('Run model:','BP','\n')
          bp_model <- BPR(train_data,test_data,target)
          bp_auc <- bp_model$auc[2]
          if(bp_auc >= 0.7)
          {
            cat('use model:','BP','\n')
            return(bp_model)
          }
          else
          {
            result_list <- list(regression=regres_model,SVM=svm_model,
                             tree=tree_model,BP=bp_model)
            auc_ <- c(regre_auc,svm_auc,tree_auc,bp_auc)
            name <- c('regression','SVM','tree','BP')
            max=which.max(auc_)
            cat('best model:',name[max],'\n')
            return(result_list[[max]])
          }
        }
      }
    }
  }
}
# DM(data, type, express = NULL , target,target_order = F)


