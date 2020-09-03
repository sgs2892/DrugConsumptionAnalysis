setwd("/Users/srujanshetty/Documents/MSinCS/Spring\ 2019/BigData/BDAnalytics/")

library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)
library(plyr)

library(e1071)

#Reading the datatable
dataTable <- read.table(
  "drug_consumption.data", 
  sep=",", header=FALSE)

#Naming the columns  
colnames(dataTable) <- c("ID", "Age","Gender","Education","Country","Ethnicity","NScore",
                         "Escore","Oscore","Ascore","Cscore","Impulsive","SS","Alcohol",
                         "Amphet","Amyl","Benzos","Caff","Cannabis","Choc","Coke",
                         "Crack","Ecstasy","Heroin","Ketamine","Legalh","LSD","Meth",
                         "Mushrooms","Nicotine","Semer","VSA")

View(dataTable)

#Renaming the coded values to make sense of dataff
dataTable$VSA <- revalue(dataTable$VSA, c("CL0"="Never Used"))
dataTable$VSA <- revalue(dataTable$VSA, c("CL1"="Used over a Decade Ago"))
dataTable$VSA <- revalue(dataTable$VSA, c("CL2"="Used in Last Decade"))
dataTable$VSA <- revalue(dataTable$VSA, c("CL3"="Used in Last Year"))
dataTable$VSA <- revalue(dataTable$VSA, c("CL4"="Used in Last Month"))
dataTable$VSA <- revalue(dataTable$VSA, c("CL5"="Used in Last Week"))
dataTable$VSA <- revalue(dataTable$VSA, c("CL6"="Used in Last Day"))
dataTable$VSA <- as.factor(dataTable$VSA)

dataTable$Semer <- revalue(dataTable$Semer, c("CL0"="Never Used"))
dataTable$Semer <- revalue(dataTable$Semer, c("CL1"="Used over a Decade Ago"))
dataTable$Semer <- revalue(dataTable$Semer, c("CL2"="Used in Last Decade"))
dataTable$Semer <- revalue(dataTable$Semer, c("CL3"="Used in Last Year"))
dataTable$Semer <- revalue(dataTable$Semer, c("CL4"="Used in Last Month"))
dataTable$Semer <- revalue(dataTable$Semer, c("CL5"="Used in Last Week"))
dataTable$Semer <- revalue(dataTable$Semer, c("CL6"="Used in Last Day"))

dataTable$Semer <- as.factor(dataTable$Semer)

dataTable$Nicotine <- revalue(dataTable$Nicotine, c("CL0"="Never Used"))
dataTable$Nicotine <- revalue(dataTable$Nicotine, c("CL1"="Used over a Decade Ago"))
dataTable$Nicotine <- revalue(dataTable$Nicotine, c("CL2"="Used in Last Decade"))
dataTable$Nicotine <- revalue(dataTable$Nicotine, c("CL3"="Used in Last Year"))
dataTable$Nicotine <- revalue(dataTable$Nicotine, c("CL4"="Used in Last Month"))
dataTable$Nicotine <- revalue(dataTable$Nicotine, c("CL5"="Used in Last Week"))
dataTable$Nicotine <- revalue(dataTable$Nicotine, c("CL6"="Used in Last Day"))
dataTable$Nicotine <- as.factor(dataTable$Nicotine)

dataTable$Mushrooms <- revalue(dataTable$Mushrooms, c("CL0"="Never Used"))
dataTable$Mushrooms <- revalue(dataTable$Mushrooms, c("CL1"="Used over a Decade Ago"))
dataTable$Mushrooms <- revalue(dataTable$Mushrooms, c("CL2"="Used in Last Decade"))
dataTable$Mushrooms <- revalue(dataTable$Mushrooms, c("CL3"="Used in Last Year"))
dataTable$Mushrooms <- revalue(dataTable$Mushrooms, c("CL4"="Used in Last Month"))
dataTable$Mushrooms <- revalue(dataTable$Mushrooms, c("CL5"="Used in Last Week"))
dataTable$Mushrooms <- revalue(dataTable$Mushrooms, c("CL6"="Used in Last Day"))
dataTable$Mushrooms <- as.factor(dataTable$Mushrooms)

dataTable$Meth <- revalue(dataTable$Meth, c("CL0"="Never Used"))
dataTable$Meth <- revalue(dataTable$Meth, c("CL1"="Used over a Decade Ago"))
dataTable$Meth <- revalue(dataTable$Meth, c("CL2"="Used in Last Decade"))
dataTable$Meth <- revalue(dataTable$Meth, c("CL3"="Used in Last Year"))
dataTable$Meth <- revalue(dataTable$Meth, c("CL4"="Used in Last Month"))
dataTable$Meth <- revalue(dataTable$Meth, c("CL5"="Used in Last Week"))
dataTable$Meth <- revalue(dataTable$Meth, c("CL6"="Used in Last Day"))
dataTable$Meth <- as.factor(dataTable$Meth)

dataTable$LSD <- revalue(dataTable$LSD, c("CL0"="Never Used"))
dataTable$LSD <- revalue(dataTable$LSD, c("CL1"="Used over a Decade Ago"))
dataTable$LSD <- revalue(dataTable$LSD, c("CL2"="Used in Last Decade"))
dataTable$LSD <- revalue(dataTable$LSD, c("CL3"="Used in Last Year"))
dataTable$LSD <- revalue(dataTable$LSD, c("CL4"="Used in Last Month"))
dataTable$LSD <- revalue(dataTable$LSD, c("CL5"="Used in Last Week"))
dataTable$LSD <- revalue(dataTable$LSD, c("CL6"="Used in Last Day"))
dataTable$LSD <- as.factor(dataTable$LSD)

dataTable$Legalh <- revalue(dataTable$Legalh, c("CL0"="Never Used"))
dataTable$Legalh <- revalue(dataTable$Legalh, c("CL1"="Used over a Decade Ago"))
dataTable$Legalh <- revalue(dataTable$Legalh, c("CL2"="Used in Last Decade"))
dataTable$Legalh <- revalue(dataTable$Legalh, c("CL3"="Used in Last Year"))
dataTable$Legalh <- revalue(dataTable$Legalh, c("CL4"="Used in Last Month"))
dataTable$Legalh <- revalue(dataTable$Legalh, c("CL5"="Used in Last Week"))
dataTable$Legalh <- revalue(dataTable$Legalh, c("CL6"="Used in Last Day"))
dataTable$Legalh <- as.factor(dataTable$Legalh)

dataTable$Ketamine <- revalue(dataTable$Ketamine, c("CL0"="Never Used"))
dataTable$Ketamine <- revalue(dataTable$Ketamine, c("CL1"="Used over a Decade Ago"))
dataTable$Ketamine <- revalue(dataTable$Ketamine, c("CL2"="Used in Last Decade"))
dataTable$Ketamine <- revalue(dataTable$Ketamine, c("CL3"="Used in Last Year"))
dataTable$Ketamine <- revalue(dataTable$Ketamine, c("CL4"="Used in Last Month"))
dataTable$Ketamine <- revalue(dataTable$Ketamine, c("CL5"="Used in Last Week"))
dataTable$Ketamine <- revalue(dataTable$Ketamine, c("CL6"="Used in Last Day"))
dataTable$Ketamine <- as.factor(dataTable$Ketamine)

dataTable$Heroin <- revalue(dataTable$Heroin, c("CL0"="Never Used"))
dataTable$Heroin <- revalue(dataTable$Heroin, c("CL1"="Used over a Decade Ago"))
dataTable$Heroin <- revalue(dataTable$Heroin, c("CL2"="Used in Last Decade"))
dataTable$Heroin <- revalue(dataTable$Heroin, c("CL3"="Used in Last Year"))
dataTable$Heroin <- revalue(dataTable$Heroin, c("CL4"="Used in Last Month"))
dataTable$Heroin <- revalue(dataTable$Heroin, c("CL5"="Used in Last Week"))
dataTable$Heroin <- revalue(dataTable$Heroin, c("CL6"="Used in Last Day"))
dataTable$Heroin <- as.factor(dataTable$Heroin)

dataTable$Ecstasy <- revalue(dataTable$Ecstasy, c("CL0"="Never Used"))
dataTable$Ecstasy <- revalue(dataTable$Ecstasy, c("CL1"="Used over a Decade Ago"))
dataTable$Ecstasy <- revalue(dataTable$Ecstasy, c("CL2"="Used in Last Decade"))
dataTable$Ecstasy <- revalue(dataTable$Ecstasy, c("CL3"="Used in Last Year"))
dataTable$Ecstasy <- revalue(dataTable$Ecstasy, c("CL4"="Used in Last Month"))
dataTable$Ecstasy <- revalue(dataTable$Ecstasy, c("CL5"="Used in Last Week"))
dataTable$Ecstasy <- revalue(dataTable$Ecstasy, c("CL6"="Used in Last Day"))
dataTable$Ecstasy <- as.factor(dataTable$Ecstasy)

dataTable$Crack <- revalue(dataTable$Crack, c("CL0"="Never Used"))
dataTable$Crack <- revalue(dataTable$Crack, c("CL1"="Used over a Decade Ago"))
dataTable$Crack <- revalue(dataTable$Crack, c("CL2"="Used in Last Decade"))
dataTable$Crack <- revalue(dataTable$Crack, c("CL3"="Used in Last Year"))
dataTable$Crack <- revalue(dataTable$Crack, c("CL4"="Used in Last Month"))
dataTable$Crack <- revalue(dataTable$Crack, c("CL5"="Used in Last Week"))
dataTable$Crack <- revalue(dataTable$Crack, c("CL6"="Used in Last Day"))
dataTable$Crack <- as.factor(dataTable$Crack)

dataTable$Coke <- revalue(dataTable$Coke, c("CL0"="Never Used"))
dataTable$Coke <- revalue(dataTable$Coke, c("CL1"="Used over a Decade Ago"))
dataTable$Coke <- revalue(dataTable$Coke, c("CL2"="Used in Last Decade"))
dataTable$Coke <- revalue(dataTable$Coke, c("CL3"="Used in Last Year"))
dataTable$Coke <- revalue(dataTable$Coke, c("CL4"="Used in Last Month"))
dataTable$Coke <- revalue(dataTable$Coke, c("CL5"="Used in Last Week"))
dataTable$Coke <- revalue(dataTable$Coke, c("CL6"="Used in Last Day"))
dataTable$Coke <- as.factor(dataTable$Coke)

dataTable$Choc <- revalue(dataTable$Choc, c("CL0"="Never Used"))
dataTable$Choc <- revalue(dataTable$Choc, c("CL1"="Used over a Decade Ago"))
dataTable$Choc <- revalue(dataTable$Choc, c("CL2"="Used in Last Decade"))
dataTable$Choc <- revalue(dataTable$Choc, c("CL3"="Used in Last Year"))
dataTable$Choc <- revalue(dataTable$Choc, c("CL4"="Used in Last Month"))
dataTable$Choc <- revalue(dataTable$Choc, c("CL5"="Used in Last Week"))
dataTable$Choc <- revalue(dataTable$Choc, c("CL6"="Used in Last Day"))
dataTable$Choc <- as.factor(dataTable$Choc)

dataTable$Cannabis <- revalue(dataTable$Cannabis, c("CL0"="Never Used"))
dataTable$Cannabis <- revalue(dataTable$Cannabis, c("CL1"="Used over a Decade Ago"))
dataTable$Cannabis <- revalue(dataTable$Cannabis, c("CL2"="Used in Last Decade"))
dataTable$Cannabis <- revalue(dataTable$Cannabis, c("CL3"="Used in Last Year"))
dataTable$Cannabis <- revalue(dataTable$Cannabis, c("CL4"="Used in Last Month"))
dataTable$Cannabis <- revalue(dataTable$Cannabis, c("CL5"="Used in Last Week"))
dataTable$Cannabis <- revalue(dataTable$Cannabis, c("CL6"="Used in Last Day"))
dataTable$Cannabis <- as.factor(dataTable$Cannabis)

dataTable$Caff <- revalue(dataTable$Caff, c("CL0"="Never Used"))
dataTable$Caff <- revalue(dataTable$Caff, c("CL1"="Used over a Decade Ago"))
dataTable$Caff <- revalue(dataTable$Caff, c("CL2"="Used in Last Decade"))
dataTable$Caff <- revalue(dataTable$Caff, c("CL3"="Used in Last Year"))
dataTable$Caff <- revalue(dataTable$Caff, c("CL4"="Used in Last Month"))
dataTable$Caff <- revalue(dataTable$Caff, c("CL5"="Used in Last Week"))
dataTable$Caff <- revalue(dataTable$Caff, c("CL6"="Used in Last Day"))
dataTable$Caff <- as.factor(dataTable$Caff)

dataTable$Benzos <- revalue(dataTable$Benzos, c("CL0"="Never Used"))
dataTable$Benzos <- revalue(dataTable$Benzos, c("CL1"="Used over a Decade Ago"))
dataTable$Benzos <- revalue(dataTable$Benzos, c("CL2"="Used in Last Decade"))
dataTable$Benzos <- revalue(dataTable$Benzos, c("CL3"="Used in Last Year"))
dataTable$Benzos <- revalue(dataTable$Benzos, c("CL4"="Used in Last Month"))
dataTable$Benzos <- revalue(dataTable$Benzos, c("CL5"="Used in Last Week"))
dataTable$Benzos <- revalue(dataTable$Benzos, c("CL6"="Used in Last Day"))
dataTable$Benzos <- as.factor(dataTable$Benzos)

dataTable$Amyl <- revalue(dataTable$Amyl, c("CL0"="Never Used"))
dataTable$Amyl <- revalue(dataTable$Amyl, c("CL1"="Used over a Decade Ago"))
dataTable$Amyl <- revalue(dataTable$Amyl, c("CL2"="Used in Last Decade"))
dataTable$Amyl <- revalue(dataTable$Amyl, c("CL3"="Used in Last Year"))
dataTable$Amyl <- revalue(dataTable$Amyl, c("CL4"="Used in Last Month"))
dataTable$Amyl <- revalue(dataTable$Amyl, c("CL5"="Used in Last Week"))
dataTable$Amyl <- revalue(dataTable$Amyl, c("CL6"="Used in Last Day"))
dataTable$Amyl <- as.factor(dataTable$Amyl)

dataTable$Amphet <- revalue(dataTable$Amphet, c("CL0"="Never Used"))
dataTable$Amphet <- revalue(dataTable$Amphet, c("CL1"="Used over a Decade Ago"))
dataTable$Amphet <- revalue(dataTable$Amphet, c("CL2"="Used in Last Decade"))
dataTable$Amphet <- revalue(dataTable$Amphet, c("CL3"="Used in Last Year"))
dataTable$Amphet <- revalue(dataTable$Amphet, c("CL4"="Used in Last Month"))
dataTable$Amphet <- revalue(dataTable$Amphet, c("CL5"="Used in Last Week"))
dataTable$Amphet <- revalue(dataTable$Amphet, c("CL6"="Used in Last Day"))
dataTable$Amphet <- as.factor(dataTable$Amphet)

dataTable$Alcohol <- revalue(dataTable$Alcohol, c("CL0"="Never Used"))
dataTable$Alcohol <- revalue(dataTable$Alcohol, c("CL1"="Used over a Decade Ago"))
dataTable$Alcohol <- revalue(dataTable$Alcohol, c("CL2"="Used in Last Decade"))
dataTable$Alcohol <- revalue(dataTable$Alcohol, c("CL3"="Used in Last Year"))
dataTable$Alcohol <- revalue(dataTable$Alcohol, c("CL4"="Used in Last Month"))
dataTable$Alcohol <- revalue(dataTable$Alcohol, c("CL5"="Used in Last Week"))
dataTable$Alcohol <- revalue(dataTable$Alcohol, c("CL6"="Used in Last Day"))
dataTable$Alcohol <- as.factor(dataTable$Alcohol)

#Splitting the data into 70% for training and 30% for cross validation.
set.seed(1234)
ind <- sample(2, nrow(dataTable), replace = T, prob = c(0.7, 0.3))
train <- dataTable[ind == 1,]
test <- dataTable[ind == 2,]

#Scaling up all the values to make them positive
dataTable$Age <- dataTable$Age + 4
dataTable$Gender <- dataTable$Gender + 4
dataTable$Education <- dataTable$Education + 4
dataTable$Country <- dataTable$Country + 4
dataTable$Ethnicity <- dataTable$Ethnicity + 4
dataTable$NScore <- dataTable$NScore + 4
dataTable$Escore <- dataTable$Escore + 4
dataTable$Oscore <- dataTable$Oscore + 4
dataTable$Ascore <- dataTable$Ascore + 4
dataTable$Cscore <- dataTable$Cscore + 4
dataTable$Impulsive <- dataTable$Impulsive + 4
dataTable$SS <- dataTable$SS + 4


#Plotting the density plots

#Plotting density plot for drug - Heroin.
#Similarly, it can be plotted for other drugs of the dataset.
dataTable %>% ggplot(aes(x=NScore , fill=Heroin))+
  geom_density(alpha=0.8,color="black")+ggtitle("Density Plot on NScore")

dataTable %>% ggplot(aes(x=Escore , fill=Heroin))+
  geom_density(alpha=0.8,color="black")+ggtitle("Density Plot on EScore")

dataTable %>% ggplot(aes(x=Oscore , fill=Heroin))+
  geom_density(alpha=0.8,color="black")+ggtitle("Density Plot on Oscore")

dataTable %>% ggplot(aes(x=Ascore , fill=Heroin))+
  geom_density(alpha=0.8,color="black")+ggtitle("Density Plot on Ascore")

dataTable %>% ggplot(aes(x=Cscore , fill=Heroin))+
  geom_density(alpha=0.8,color="black")+ggtitle("Density Plot on Cscore")

dataTable %>% ggplot(aes(x=Gender , fill=Heroin))+
  geom_density(alpha=0.8,color="black")+ggtitle("Density Plot on Gender")

dataTable %>% ggplot(aes(x=SS , fill=Heroin))+
  geom_density(alpha=0.8,color="black")+ggtitle("Density Plot on SS")

dataTable %>% ggplot(aes(x=Ethnicity , fill=Heroin))+
  geom_density(alpha=0.8,color="black")+ggtitle("Density Plot on Ethnicity")

dataTable %>% ggplot(aes(x=Education , fill=Heroin))+
  geom_density(alpha=0.8,color="black")+ggtitle("Density Plot on Education")

dataTable %>% ggplot(aes(x=Impulsive , fill=Heroin))+
  geom_density(alpha=0.8,color="black")+ggtitle("Density Plot on Impulsive")

#Function to peform Naive Baye's Classification
classification = function(drug_name){
  NBClassifier=naiveBayes(drug_name ~ dataTable$Age+dataTable$Gender+dataTable$Education+dataTable$Country+
                            dataTable$Ethnicity+dataTable$NScore+dataTable$Escore+dataTable$Oscore+dataTable$Ascore+
                            dataTable$Cscore+dataTable$Impulsive+dataTable$SS, data = train)
  print(NBClassifier)
  return(NBClassifier) 
}


#Function to find Acuracy and perform cross validation on data.
printALL=function(model,training,testing){
  trainPred=predict(model, newdata = train, type = "class")
  trainTable=table(training, trainPred)
  testPred=predict(model, newdata=test, type="class")
  testTable=table(testing, testPred)
  trainAcc = (trainTable[1,1]+trainTable[2,2]+trainTable[3,3]+trainTable[4,4]+trainTable[5,5]+trainTable[6,6])/sum(trainTable)
  #trainAcc = diag(trainTable)/sum(trainTable)
  testAcc = (testTable[1,1]+testTable[2,2]+testTable[3,3]+testTable[4,4]+testTable[5,5]+testTable[6,6])/sum(testTable)
  #testAcc = diag(testTable)/sum(testTable)
  message("Contingency Table for Training Data")
  print(trainTable)
  message("Contingency Table for Test Data")
  print(testTable)
  message("Accuracy")
  print(round(cbind(trainAccuracy=trainAcc, testAccuracy=testAcc),3))
  train_misclassification = 1-trainAcc
  test_misclassification = 1 - testAcc
  print(round(cbind(trainmisclassification=train_misclassification, testmisclassification=test_misclassification),3))
}


#Function Calls
result=classification(dataTable$Heroin)
printALL(result,train$Heroin,test$Heroin)

result=classification(dataTable$Amphet)
printALL(result,train$Amphet,test$Amphet)

result=classification(dataTable$Amyl)
printALL(result,train$Amyl,test$Amyl)

result=classification(dataTable$Benzos)
printALL(result,train$Benzos,test$Benzos)

result=classification(dataTable$Coke)
printALL(result,train$Coke,test$Coke)

result=classification(dataTable$Caff)
printALL(result,train$Caff,test$Caff)

result=classification(dataTable$Cannabis)
printALL(result,train$Cannabis,test$Cannabis)

result=classification(dataTable$Choc)
printALL(result,train$Choc,test$Choc)

result=classification(dataTable$Crack)
printALL(result,train$Coke,test$Crack)

result=classification(dataTable$Ecstasy)
printALL(result,train$Ecstasy,test$Ecstasy)

result=classification(dataTable$Alcohol)
printALL(result,train$Alcohol,test$Alcohol)

result=classification(dataTable$Ketamine)
printALL(result,train$Ketamine,test$Ketamine)

result=classification(dataTable$Legalh)
printALL(result,train$Legalh,test$Legalh)

result=classification(dataTable$LSD)
printALL(result,train$LSD,test$LSD)

result=classification(dataTable$Meth)
printALL(result,train$Meth,test$Meth)

result=classification(dataTable$Mushrooms)
printALL(result,train$Mushrooms,test$Mushrooms)

result=classification(dataTable$Nicotine)
printALL(result,train$Nicotine,test$Nicotine)

result=classification(dataTable$Semer)
printALL(result,train$Semer,test$Semer)

result=classification(dataTable$VSA)
printALL(result,train$VSA,test$VSA)