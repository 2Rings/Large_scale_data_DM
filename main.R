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
        svm_model <- SVM(train_data,test_data,target,type)
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
      svm_model <- SVM(train_data,test_data,target,type)
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


