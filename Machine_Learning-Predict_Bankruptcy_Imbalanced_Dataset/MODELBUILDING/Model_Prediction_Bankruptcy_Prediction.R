//setwd('C:/Users/Aerow/Desktop/CUTE_3/CUTE_3')
library(randomForest)
library(DMwR)
library(caret)
library(xgboost)
rm(list=ls(all.names = T))
train_data<-read.csv('train.csv')
colSums(is.na(train_data))
train_nA_omit<-na.omit(train_data)
test_data<-read.csv('test.csv')
test_imp<-knnImputation(data = test_data,k=5)
colSums(is.na(test_imp))
test_std<-preProcess(test_imp,method=c('center','scale'))
test_final<-predict(test_std,test_imp)
train_data[,c('ID')]=NULL
train_data_imp<-centralImputation(train_data)
train_data_imp$target<-ifelse(train_data_imp$target==0,'n','y')

train_std<-preProcess(train_data_imp,method = c('center','scale'))
train_std_data<-predict(train_std,train_data_imp)

sampling_strategy <- trainControl(method = "repeatedcv", number = 5, repeats = 2, verboseIter = F, allowParallel = T)

param_grid <- expand.grid(.nrounds = 20, .max_depth = c(2, 4, 6), .eta = c(0.1, 0.3),
                          .gamma = c(0.6, 0.5, 0.3), .colsample_bytree = c(0.6, 0.4),
                          .min_child_weight = 1, .subsample = c(0.5, 0.6, 0.9))

xgb_tuned_model <- train(x = train_std_data[ , -65], 
                         y = train_std_data[ , 65], 
                         method = "xgbTree",
                         trControl = sampling_strategy,
                         tuneGrid = param_grid)

preds<-predict(xgb_tuned_model,test_final)
preds<-as.numeric(preds)
preds<-ifelse(preds==0,0,1)
preds_p<-data.frame(preds)
preds_p[,2]<-test_data[,'ID']
preds_p[,3]<-seq(1,nrow(test_data),1)
colnames(preds_p)=c('prediction','ID','dum')
write.table('submission.csv',x = preds_p[c('dum','ID','prediction')],sep = ",",col.names = c('','ID','prediction'))


variable_importance_matrix <- xgb.importance(feature_names = colnames(train_std_data), model = xgb_tuned_model$finalModel)

xgb.plot.importance(variable_importance_matrix)
s<-as.list(variable_importance_matrix[variable_importance_matrix$Importance>0.02,1])



sampling_strategy <- trainControl(method = "repeatedcv", number = 5, repeats = 2, verboseIter = F, allowParallel = T)

param_grid <- expand.grid(.nrounds = 20, .max_depth = c(2, 4, 6), .eta = c(0.1, 0.3),
                          .gamma = c(0.6, 0.5, 0.3), .colsample_bytree = c(0.6, 0.4),
                          .min_child_weight = 1, .subsample = c(0.5, 0.6, 0.9))

xgb_tuned_model <- train(x = train_std_data[ , s[[1]]], 
                         y = train_std_data[ , 65], 
                         method = "xgbTree",
                         trControl = sampling_strategy,
                         tuneGrid = param_grid)

preds<-predict(xgb_tuned_model,test_final[,s[[1]]])
preds<-as.numeric(preds)
preds<-ifelse(preds==1,0,1)
preds_p<-data.frame(preds)
preds_p[,2]<-test_data[,'ID']
preds_p[,3]<-seq(1,nrow(test_data),1)
colnames(preds_p)=c('prediction','ID','dum')
write.table('submission.csv',x = preds_p[c('dum','ID','prediction')],sep = ",",col.names = c('','ID','prediction'))

