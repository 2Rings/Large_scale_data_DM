head(train_data); head(test_data)
type;  name <- colnames(train_data); target
sapply(train_data, class)
sapply(test_data, class)
train_data[] <- sapply(train_data,as.numeric)
test_data[] <- sapply(test_data,as.numeric)

targetIndex = which(name == target)
unique(train_data[,target]); unique(test_data[,target])
if (type[targetIndex] == 1)
{
  train_data[,target] = train_data[[ncol(train_data)]] - 1
  test_data[,target] = test_data[[ncol(test_data)]] - 1
  unique(train_data[,target]); unique(test_data[,target])
}


write.csv(target,file='target.csv',row.names = F)
write.csv(test_data,file='test_data.csv',row.names = F)
write.csv(train_data,file='train_data.csv',row.names = F)
write.csv(type,file='type.csv',row.names = F)
write.csv(name,file='name.csv',row.names = F)


