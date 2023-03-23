#install.packages(c("dplyr", "psych", "ggplot2","caret","klaR","e1071"))
library(dplyr)
library(psych)
library(ggplot2)
library(caret)
library(klaR)
library(e1071)

#set working directory and read data
data<-read.table("heart_2020_cleaned.csv",header = TRUE,sep=",")

#filter data to HeartDisease,Smoking, AgeCategory, Sex, BMI, Diabetic, SleepTime, KidneyDisease
var<- as.vector(c("HeartDisease", "Smoking", "AgeCategory", "Sex", "BMI", "Diabetic", "SleepTime", "KidneyDisease"))
cdata<- data[,var]

testdata<-as.data.frame(cdata[100001:101001,])
testdata$SleepTime<-factor(testdata$SleepTime, levels=1:100)
traindata<-as.data.frame(cdata[1:100000,])
traindata$SleepTime<-factor(traindata$SleepTime, levels=1:100)
model<-naiveBayes(HeartDisease~Smoking + AgeCategory + Sex + BMI + Diabetic + SleepTime + KidneyDisease,traindata)
model

x_test <- testdata[,2:6]
y_test <- testdata[,1]
predictions <- predict(model,x_test)
cm<-table(testdata$HeartDisease, predictions)

confusionMatrix(cm)
