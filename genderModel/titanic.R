# Kaggle Titanic project

## set wd
setwd('/Users/snowleopard/Documents/workspace/kaggle/titanic/genderModel')

## clear workspace
closeAllConnections();
rm(list=ls())

#### Part 1 - Loading data & basic analysis #####

## load data
trainData <- read.csv('../data/train.csv', header = T, stringsAsFactors = FALSE)
testData <- read.csv('../data/test.csv', header = T, stringsAsFactors = FALSE)

prop.table(table(trainData$Sex, trainData$Survived),1)
testData$Survived <- 0
testData$Survived[testData$Sex == 'female'] = 1

summary(trainData$Age)

## New variable child i.e. age < 18
trainData$Child <- 0
trainData$Child[trainData$Age < 18] = 1
aggregate(Survived ~ Child + Sex, data = trainData, FUN=function(x) {sum(x)/length(x)})

## new variable fare class
trainData$Fare2 = '30+'
trainData$Fare2[trainData$Fare < 30 & trainData$Fare >= 20] = '20-30'
trainData$Fare2[trainData$Fare < 20 & trainData$Fare >= 10] = '10-20'
trainData$Fare2[trainData$Fare < 10] = '<10'
aggregate(Survived ~ Fare2 + Pclass + Sex, data = trainData, FUN=function(x) {sum(x)/length(x)})

## Predicting that all female survived, except those Pclass = 3 and fare >= 20
testData$Survived = 0
testData$Survived[testData$Sex == 'female'] = 1
testData$Survived[testData$Sex == 'female' & testData$Pclass == 3 & testData$Fare >= 20] = 0

submit <- data.frame(PassengerId = testData$PassengerId, Survived = testData$Survived)
write.csv(submit,file = 'theyallperish.csv', row.names = FALSE)
