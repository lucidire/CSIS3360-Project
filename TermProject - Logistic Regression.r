#references:
#https://stats.stackexchange.com/questions/453041/selecting-categorical-and-continuous-variable-for-logistic-regression
#https://www.geeksforgeeks.org/regression-with-categorical-variables-in-r-programming/

#install and applay packages for Naive Bayes analaysis
install.packages(c("caret","e1071"))
library(caret)
library(e1071)

#set working directory and read data
data<-read.table("heart_2020_cleaned.csv",header = TRUE,sep=",")

#filter data to HeartDisease,Smoking, AgeCategory, Sex, BMI, Diabetic, SleepTime, KidneyDisease
var<- as.vector(c("HeartDisease", "Smoking", "AgeCategory", "Sex", "BMI", "Diabetic", "SleepTime", "KidneyDisease"))
cdata<- data[,var]

#transform data into 0's and 1s for analysis
#Sex - Female = 0, Male = 1
#Diabetes - Yes and Yes (during pregnancy) = 1, Otherwise = 0
#Change AgeCategory called Old where Old means 60 years

cdata$HeartDisease[cdata$HeartDisease=="Yes"]<-1
cdata$HeartDisease[cdata$HeartDisease=="No"]<-0
cdata$Smoking[cdata$Smoking=="Yes"]<-1
cdata$Smoking[cdata$Smoking=="No"]<-0
cdata$Diabetic<-with(cdata, ifelse(Diabetic == "Yes" | Diabetic == "Yes (during pregnancy)", 1,0))
cdata$KidneyDisease[cdata$KidneyDisease=="Yes"]<-1
cdata$KidneyDisease[cdata$KidneyDisease=="No"]<-0
cdata$Sex<-with(cdata,ifelse(Sex == "Female",0,1))
cdata$Old<-with(cdata, ifelse(AgeCategory == "65-69" | 
                                AgeCategory == "70-74"|
                                AgeCategory == "75-79"|
                                AgeCategory == "80 or older",
                              1,0))

#remove AgeCategory
cdata<-cdata[,-3]

#convert data types into numeric
c_name<-c("HeartDisease", "Smoking",  "Sex", "BMI", "Diabetic", "SleepTime", "KidneyDisease","Old")
cdata[c_name] <- sapply(cdata[c_name],as.numeric)
sapply(cdata, class)

#separating data 
testdata<-as.data.frame(cdata[100001:319795,])
traindata<-as.data.frame(cdata[1:100000,])


#create a Logistic Regression
lmodel<-glm(HeartDisease~Smoking + Old + Sex + BMI + Diabetic + SleepTime + KidneyDisease,data=traindata, family = binomial)

#output
summary(lmodel)

#predictions
p1 <- predict(lmodel, testdata, type = 'response')

#confusion matrix
pre1<-ifelse(p1 > 0.5, 1, 0)
table<-table(Prediction = pre1, Actual = testdata$HeartDisease)

#error calculation as decimal
1 - sum(diag(table)) / sum(table)
