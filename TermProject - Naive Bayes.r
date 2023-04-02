#install and applay packages for Naive Bayes analaysis
install.packages(c("caret","e1071"))
library(caret)
library(e1071)

#set working directory and read data
data<-read.table("heart_2020_cleaned.csv",header = TRUE,sep=",")

#filter data to HeartDisease,Smoking, AgeCategory, Sex, BMI, Diabetic, SleepTime, KidneyDisease
var<- as.vector(c("HeartDisease", "Smoking", "AgeCategory", "Sex", "BMI", "Diabetic", "SleepTime", "KidneyDisease"))
cdata<- data[,var]

#separating data into test and training data 
testdata<-as.data.frame(cdata[100001:319795,])
testdata$SleepTime<-factor(testdata$SleepTime, levels=0:24)
traindata<-as.data.frame(cdata[1:100000,])
traindata$SleepTime<-factor(traindata$SleepTime, levels=0:24)

#create a Naive Bayes model
nmodel<-naiveBayes(HeartDisease~Smoking + AgeCategory + Sex + BMI + Diabetic + SleepTime + KidneyDisease,traindata)
nmodel

#test Naive Bayes model
x_test <- testdata[,2:6]
y_test <- testdata[,1]
predictions <- predict(nmodel,x_test)
cm<-table(testdata$HeartDisease, predictions)

#check accuracy on Naive Bayes
confusionMatrix(cm)
