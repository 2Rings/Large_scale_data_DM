# ------------------------------------------------------------------
# 需要提供的参数：DATA，type，express,target
# 指定缺失值所占比例达到ratio时被删除
# 指定缺失值的表示express;out参数表示是否以文件的形式输出
# ---------------------------------------------------------
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

# 将名义型变量数值化-------------------------------------
category_to_numeric <- function(train_data, test_data, type, out = T)
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
  for(j in seq(ncol(train_data)))
  {
    if (type[j] == 0)
    {
      train_data[ ,j] <- as.numeric(train_data[[j]])
      test_data[ ,j] <- as.numeric(test_data[[j]])
    }
    else
    {
      train_data[ ,j] <- as.factor(train_data[[j]])
      test_data[ ,j] <- as.factor(test_data[[j]])
      levels(test_data[ ,j]) <- levels(train_data[[j]])
    }
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
