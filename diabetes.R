#load the diabetes dataset

diabetes<-read.csv("diabetes.csv",stringsAsFactors = FALSE)

str(diabetes)

#load the needed libraries

library(dplyr)
library(tidyverse)

#our aim is to predict whether the person is diabetic or not

#since its classification change the outcome to factor

dim(diabetes)

diabetes$Outcome<-as.factor(diabetes$Outcome)

str(diabetes)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


data_nom<-as.data.frame(lapply(diabetes[1:8],normalize))
summary(data_nom)

diabetes2<-cbind(data_nom,diabetes[9])
train<-diabetes2[1:460,]
dim(train)
test<-diabetes2[461:768,]
dim(test)
#sum(is.na(diabetes))
model1<-glm(Outcome~Glucose+Insulin+BMI+DiabetesPedigreeFunction+Age,data = train,family = "binomial"
            )

pred<-predict(model1,data = test,type = "response")
change<-ifelse(pred>0.5,1,0)

change_fact<-factor(change,levels = c(0,1))

table(change_fact,train$Outcome)

mean(change_fact==train$Outcome)


library(randomForest)
mr<-randomForest(train$Outcome~train$Glucose+train$Insulin+train$BMI+train$DiabetesPedigreeFunction+train$Age,data = train,ntree = 500)
pr<-predict(mr,test)
table(pr,test$Outcome)
mean(pr== data3te$diagnosis)


library(rpart)
rd<-rpart(Outcome~.,data = train,method = "class")
pd<-predict(rd,test,type = "class")
table(pd,test$Outcome)

mean(pd == test$Outcome)


library(naivebayes)
m2<-naive_bayes(Outcome~.,data = train)
m2
predn<-predict(m2,test$Outcome,type= "prob",laplace = 1)
table(test$Outcome,m2)

library(class)
train_k<-data_nom[1:460,]
test_k<-data_nom[461:708,]

train_k_label<-diabetes[1:460,9]
view(train_k_label)
test_k_label<-diabetes[461:708,9]

kmodel<-knn(train_k,test_k,cl = train_k_label,k = 5)

table(test_k_label,kmodel)

mean(test_k_label==kmodel)
# so stressing not done yet
