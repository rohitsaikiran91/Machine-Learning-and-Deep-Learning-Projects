Machines<-read.csv("Machine_train.csv")
gtrain_data<-merge(train_data,Machines,all.x = T)
K<-train_data[c("MachineID")]
train_data$MachineID=NULL
train_data$MachineModel=NULL
str(train_data)
table(train_data$ActionPoint)
library(DMwR)
library(caret)
library(glmnet)
train_data[is.na(train_data)]<-0
############################Elastic net with PCA####################################
train_pre<-preProcess(train_data[-1],method = c("center","scale","pca"))
train_data_f<-predict(train_pre,train_data)
train_data_f["target"]<-train_data$ActionPoint

test_d<-predict(train_pre,test_d)
smote_in<-DMwR::SMOTE(ActionPoint~.,data = train_data,perc.over = (670/70)*100,perc.under=(200/70)*100,k=5)
table(smote_in$ActionPoint)


sampling_Elastic<-trainControl(method = 'repeatedcv',number = 5,repeats = 5,verboseIter = T,sampling = "down")
param_elastic<-expand.grid(.lambda=seq(from=0,to = 10,by = 0.5),.alpha=seq(from = 0,to = 1,by = 0.05))
Elastic_net<-caret::train(ActionPoint~.,data=train_data_f,method='glmnet',trControl = sampling_Elastic,tuneGrid = param_elastic,metric = 'Accuracy',family="multinomial")

preds<-predict(Elastic_net,test_data[,-1])
confusionMatrix(data = preds,reference = test_data$ActionPoint)

test_d<-read.csv("test.csv")
test_d<-merge(test_d,Machines,all.x = T)
F<-test_d$MachineID
test_d$MachineID=NULL
preds_test<-predict(Elastic_net,test_d)
table(preds_test)
preds_test<-as.character(preds_test)
submission<-data.frame(cbind(as.character(F),preds_test))

colnames(submission) = c("MachineID","ActionPoint")
write.table(submission,file = "sample_submission.csv",sep =",", row.names = T,col.names = NA)
#############################XGBoost with weights###########################################
train_data<-read.csv("train.csv")
train_data<-merge(train_data,Machines,all.x = T)
train_data$MachineID=NULL
str(train_data)
train_data[is.na(train_data)]<-0
temp<-train_data$ActionPoint
dum<-dummyVars(~.,data = train_data[,-1])
train_data<-as.data.frame(predict(dum,train_data))
train_data["ActionPoint"]<-temp

index_data<-createDataPartition(p = 0.7,y = train_data$ActionPoint)
t_data<-train_data[index_data$Resample1,]
te_data<-train_data[-index_data$Resample1,]

table(t_data$ActionPoint)
table(te_data$ActionPoint)

a=0.1
b=0.1
c=100

class_weights<-ifelse(t_data$ActionPoint=="ComponentRepair",a,ifelse(t_data$ActionPoint=="NoIssue",b,c))

sampling_strategy <- trainControl(method = "repeatedcv", number = 2, repeats = 2, allowParallel = T,classProbs=T)

param_grid <- expand.grid(.nrounds = 20, .max_depth = c(2, 4, 6), .eta = c(0.1, 0.3),
                          .gamma = c(0.6, 0.5, 0.3), .colsample_bytree = c(0.9,0.6),
                          .min_child_weight = 1, .subsample = c(0.5, 0.6, 0.9))

xgb_tuned_model <- train(x = t_data[ , -32], 
                         y = t_data[ , 32], 
                         method = "xgbTree",
                         trControl = sampling_strategy,
                         tuneGrid = param_grid,weights =class_weights,metric = "ROC")
preds<-predict(xgb_tuned_model,te_data[,-32])
preds_train<-predict(xgb_tuned_model,t_data[,-32])
confusionMatrix(data = preds,reference = te_data$ActionPoint)
confusionMatrix(data = preds_train,reference = t_data$ActionPoint)


test_d<-read.csv("test.csv")
test_d<-merge(test_d,Machines,all.x = T)
F<-test_d$MachineID
test_d$MachineID=NULL
dum_t<-dummyVars(~.,data = test_d)
test_d<-as.data.frame(predict(dum_t,test_d))

preds_test<-predict(xgb_tuned_model,test_d)
table(preds_test)
preds_test<-as.character(preds_test)
submission<-data.frame(cbind(as.character(F),preds_test))

colnames(submission) = c("MachineID","ActionPoint")
write.table(submission,file = "sample_submission.csv",sep =",", row.names = T,col.names = NA)


#################################SVM
train_data<-read.csv("train.csv")
train_data<-merge(train_data,Machines,all.x = T)
train_data$MachineID=NULL
train_data[is.na(train_data)]<-0

train_data1<-train_data
train_data2<-train_data
train_data3<-train_data
train_data1$ActionPoint<-ifelse(train_data$ActionPoint=="ComponentReplacement","ComponentReplacement","Other")
train_data2$ActionPoint<-ifelse(train_data$ActionPoint=="NoIssue","NoIssue","Other")
train_data3$ActionPoint<-ifelse(train_data$ActionPoint=="ComponentRepair","ComponentRepair","Other")

table(train_data1$ActionPoint)
table(train_data2$ActionPoint)
table(train_data3$ActionPoint)
sampling_strategy <- trainControl(method = "repeatedcv", number = 4, repeats = 4,sampling = "up")
sampling_strategy1 <- trainControl(method = "repeatedcv", number = 4, repeats = 4,"up")
param_grid<-expand.grid(.C= c(0.1,10,10^-4, 10^-3, 10^-2, 10^-1, 10^1, 10^2, 10^3),.degree=c(2,3,4,5),.scale = c(0.15, 0.25, 1))
Model1<- train(ActionPoint ~ . , train_data1, method = "svmPoly",
                           tuneGrid = param_grid, trControl = sampling_strategy)

Model2<- train(ActionPoint ~ . , train_data2, method = "svmLinear",
               tuneGrid = data.frame(.C=c(0.01,10,10^-4, 10^-3, 10^-2, 10^-1, 10^1, 10^2, 10^3)), trControl = sampling_strategy1)

Model3<- train(ActionPoint ~ . , train_data3, method = "svmPoly",
               tuneGrid = param_grid, trControl = sampling_strategy)

str(train_data1)
pred_model1<-predict(Model1,train_data1[,-1])
pred_model2<-predict(Model2,train_data2[,-1])
pred_model3<-predict(Model3,train_data3[,-1])
table(pred_model3)
confusionMatrix(data = pred_model3,reference = as.factor(train_data3$ActionPoint))
train_stack<-as.data.frame(cbind(as.character(pred_model1),as.character(pred_model2),as.character(pred_model3)))
table(submission$V2)
sum(is.na(train_stack))

test_d<-read.csv("test.csv")
test_d<-merge(test_d,Machines,all.x = T)
TF<-test_d$MachineID
test_d$MachineID=NULL
test_d[is.na(test_d)]<-0
pred1_test<-as.character(predict(Model1,test_d))
pred2_test<-as.character(predict(Model2,test_d))
pred3_test<-as.character(predict(Model3,test_d))
table(pred1_test)
table(pred2_test)
table(pred3_test)

train_stack["Target"]<-train_data$ActionPoint
test_stack<-as.data.frame(cbind(pred1_test,pred2_test,pred3_test))

index_data1<-createDataPartition(p = 0.7,y = train_stack$Target)
t_data<-train_stack[index_data1$Resample1,]
te_data<-train_stack[-index_data1$Resample1,]
##################################Stack
a=0.1
b=0.1
c=3

class_weights<-ifelse(F1=="ComponentRepair",a,ifelse(F1=="NoIssue",b,c))

dum<-dummyVars(~.,data = t_data[,-4])
F1<-t_data$Target
t_data<-predict(dum,t_data)
t_data<-as.data.frame(t_data)
dum_t<-dummyVars(~.,data = te_data[,-4])
S<-te_data$Target
te_data<-as.data.frame(predict(dum_t,te_data))
te_data["Target"]<-S
sampling_strategy <- trainControl(method = "repeatedcv", number = 2, repeats = 2, allowParallel = T)

param_grid <- expand.grid(.nrounds = 20, .max_depth = c(2, 4, 6), .eta = c(0.1, 0.3),
                          .gamma = c(0.6, 0.5, 0.3), .colsample_bytree = c(0.9,0.6),
                          .min_child_weight = 1, .subsample = c(0.5, 0.6, 0.9))
F1
xgb_tuned_model <- train(x = t_data[,-7], 
                         y = F1, 
                         method = "xgbTree",
                         trControl = sampling_strategy,
                         tuneGrid = param_grid,weights =class_weights,metric = "Accuracy")
preds<-predict(xgb_tuned_model,te_data[,-7])
preds_train<-predict(xgb_tuned_model,t_data[,-32])
confusionMatrix(data = preds,reference = te_data$Target)
confusionMatrix(data = preds_train,reference = t_data$ActionPoint)


##################################Full Model Stack#######################
dum<-dummyVars(~.,train_stack[,-4])
FI<-train_data$ActionPoint
train_stack<-as.data.frame(predict(dum,train_stack[,-4]))
dum_t<-dummyVars(~.,test_stack)
test_stack<-as.data.frame(test_stack)


a=0.1
b=0.1
c=1

class_weights<-ifelse(FI=="ComponentRepair",a,ifelse(FI=="NoIssue",b,c))

sampling_strategy <- trainControl(method = "repeatedcv", number = 5, repeats = 2, allowParallel = T)

param_grid <- expand.grid(.nrounds = 20, .max_depth = c(2, 4, 6), .eta = c(0.1, 0.3),
                          .gamma = c(0.6, 0.5, 0.3), .colsample_bytree = c(0.9,0.6),
                          .min_child_weight = 1, .subsample = c(0.5, 0.6, 0.9))
length(F)
xgb_model_final <- train(x = train_stack, 
                         y = F, 
                         method = "xgbTree",
                         trControl = sampling_strategy,
                         tuneGrid = param_grid,weights =class_weights,metric = "Accuracy")

preds_test<-predict(xgb_model_final,test_stack)
table(preds_test)
preds_test<-as.character(preds_test)

submission<-data.frame(cbind(as.character(F),preds_test))

colnames(submission) = c("MachineID","ActionPoint")
write.table(submission,file = "sample_submission.csv",sep =",", row.names = T,col.names = NA)