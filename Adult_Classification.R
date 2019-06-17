library(ggplot2)
library(gridExtra)
library(caret)
library(rpart)
library(dplyr)
library(caTools)

#Reads and summarizes the dataset
income = read.csv('Desktop/adult.csv')
head(income)
str(income)
summary(income)

table(income$workclass)

#Filters the unemployment category between unemployed and employed
unemployed <- function(job){
  job <- as.character(job)
  if(job=='Never-worked' | job=='Without-pay'){
    return('Unemployed')
  }else{
    return(job)
  }
}
income$workclass <- sapply(income$workclass,unemployed)
table(income$workclass)

#Filters the marriage category between not married, nevermarried, and married 
marriage <- function(status){
  status <- as.character(status)
  if(status=='Never-married'){
    return(status)
  }else if(status=='Separated' | status=='Divorced' | status=='Widowed'){
    return('Not-Married')
  }else{
    return('Married')
  }
}
income$marital.status <- sapply(income$marital.status,marriage)
table(income$marital.status)

#Applies the changes to the table and factors them in 
income$workclass <- sapply(income$workclass,factor)
income$marital.status <- sapply(income$marital.status,factor)

str(adult)

#Plots a histogram of the income and the age
ggplot(income,aes(age)) + geom_histogram(aes(fill=income),color='black',binwidth=1) + theme_bw()

#Splits the dataset so it can be trained and tested on the remaining
split <- sample.split(income$income, SplitRatio = 0.7)
train <- subset(income, split == TRUE)
test <- subset(income, split == FALSE)

#Classifies the data with the binomial regression
output <- step(glm(income~.,data=train,family = "binomial"),direction = "backward")
summary(classifier)

#Makes the prediction based on the output of the binomial regression machine learning function 
prediction <-predict(output,test, type = "response")

predicted = table(Actual=test$income, prediction= prediction >= 0.5)
predicted

#Outputs the percentage correct of the predictions
sum(diag(predicted)/sum(predicted))

