library(data.table)
require(arules)
library(tidyverse)
library(Jmisc)
library(imputeTS)
library(lubridate)
library(glmnet)
library(penalized)
library(ISLR)
library(ie2misc)
library(caret)
library(randomForest)
library(rpart)

test=read.table("test.csv",header=TRUE, sep=',')
train=read.table("train.csv",header=TRUE, sep=',')

test<-test[,-1]
train<-train[,-1]

x_train<-train[,1:27]
y_train<- array(numeric(),c(nrow(x_train),1))
y_train[,1]<-train[,28]
x_train_scale<-scale(x_train, center = TRUE, scale = TRUE)

x_test<-test[,1:27]
y_test<- array(numeric(),c(nrow(x_test),1))
y_test[,1]<-test[,28]
x_test_scale<-scale(x_test, center = TRUE, scale = TRUE)

alpha_fit <- cv.glmnet(x_train_scale, y_train, alpha = 1, family="gaussian",type.measure = "mse")
alpha_predicted<-predict(alpha_fit, s=alpha_fit$lambda.1se,newx=x_test_scale)
mean_lasso<-mean((y_test-alpha_predicted)^2)


####GBM

gbm_fit<-train(total_goals~.,data=train,method='gbm',trControl=trainControl(method = 'cv'))

gbm_fit$bestTune

plot(gbm_fit)

summary(gbm_fit)

View(cbind(estimate_gbm,y_test,alpha_predicted))
estimate_gbm=predict(gbm_fit,test,n.trees = 150)
mean_gbm<-mean((y_test-estimate_gbm)^2)

###Random Forest

rf<-randomForest(total_goals~.,data=train)
plot(rf)
importance(rf)

rf$ntree

est_rf=predict(rf,test,n.trees = 150,type='response')

mean_rf<-mean((y_test-est_rf)^2)


###Decision Trees

tree=rpart(total_goals~.,train,
           method='anova',
           control=rpart.control(cp=0.01))

est_dt=predict(tree,test)

mean_dt<-mean((y_test-est_dt)^2)

#### PREDICTION OF CLASS
#### PREDICTION OF CLASS

test_class=read.table("test.csv",header=TRUE, sep=',')
train_class=read.table("train.csv",header=TRUE, sep=',')

test_class<-test_class[,-1]
train_class<-train_class[,-1]

train_class$over<-ifelse(train_class$total_goals>2.5,yes=1,no=0)
test_class$over<-ifelse(test_class$total_goals>2.5,yes=1,no=0)

train_class<-train_class[,-28]
test_class<-test_class[,-28]
x_train_class<-train_class[,1:27]
y_train_class<- array(numeric(),c(nrow(x_train_class),1))
y_train_class[,1]<-train_class[,28]
x_train_class_scale<-scale(x_train_class, center = TRUE, scale = TRUE)

x_test_class<-test_class[,1:27]
y_test_class<- array(numeric(),c(nrow(x_test_class),1))
y_test_class[,1]<-test_class[,28]
x_test_class_scale<-scale(x_test_class, center = TRUE, scale = TRUE)

alpha_fit_class <- cv.glmnet(x_train_class_scale, y_train_class, alpha = 1, family="gaussian",type.measure = "mse")
alpha_predicted_class<-predict(alpha_fit_class,s=alpha_fit_class$lambda.1se,newx=x_test_class_scale)

## ACCURACY
acc_1=data.frame(cbind(y_test_class,alpha_predicted_class))
COUNT=0
for (i in 1:nrow(acc_1)){if (acc_1[i,1]!=acc_1[i,2]){
  COUNT=COUNT +1
  COUNT
}}
accuracy_lasso=COUNT/nrow(acc_1)
accuracy_lasso

####GBM
train_class$over<-as.factor(train_class$over)
test_class$over<-as.factor(test_class$over)

gbm_fit_class<-train(over~.,data=train_class,method='gbm',trControl=trainControl(method = 'cv'))
estimate_gbm_class=predict(gbm_fit_class,test_class[,-28],n.trees = 100)## 1 class 0, 2 class 1


acc_2=data.frame(cbind(y_test_class,estimate_gbm_class))
COUNT=0
for (i in 1:nrow(acc_2)){if (acc_2[i,1]!=acc_2[i,2]){
  COUNT=COUNT +1
  COUNT
}}
accuracy_gbm=COUNT/nrow(acc_2)
accuracy_gbm

###Random Forest

rf_class<-randomForest(over~.,data=train_class)
est_rf_class=predict(rf_class,test_class,n.trees = 100,type='response')
## ACCURACY 

acc_3=data.frame(cbind(y_test_class,est_rf_class))
COUNT=0
for (i in 1:nrow(acc_3)){if (acc_3[i,1]!=acc_3[i,2]){
  COUNT=COUNT +1
  COUNT
}}
accuracy_rf=COUNT/nrow(acc_3)
accuracy_rf 

###Decision Trees

tree_class=rpart(over~.,train_class,method='class',control=rpart.control(cp=0.01))

est_dt_class=predict(tree_class,test_class,type='class')

## ACCURACY 
acc_4=data.frame(cbind(y_test_class,est_dt_class))
COUNT=0
for (i in 1:nrow(acc_4)){if (acc_4[i,1]!=acc_4[i,2]){
  COUNT=COUNT +1
  COUNT
}}
accuracy_dt=COUNT/nrow(acc_4)
accuracy_dt
