# Kaggle Titanic project using Decision Trees

## set wd
setwd('/Users/snowleopard/Documents/workspace/kaggle/titanic/decisionTrees')

## clear workspace
closeAllConnections();
rm(list=ls())

#### Part 1 - Loading data & basic analysis #####

## load data
trainData <- read.csv('../data/train.csv', header = T, stringsAsFactors = FALSE)
testData <- read.csv('../data/test.csv', header = T, stringsAsFactors = FALSE)

library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

## Feature engineering
testData$Survived <- NA
combi <- rbind(trainData, testData)

### new column title - mr, mrs, etc
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split = '[,.]')[[1]][2]})
combi$Title <- sub(' ','', combi$Title) # remove spaces before the title

#### comnbine common titles
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

### new col family size
combi$FamilySize <- combi$SibSp + combi$Parch + 1

### new col surname
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split = '[,.]')[[1]][1]})

### new col familyId - familySize + surname
combi$FamilyId <- paste0(combi$FamilySize, combi$Surname)
combi$FamilyId[combi$FamilySize <= 2] <- 'Small'

### three family size cut off
famIds <- data.frame(table(combi$FamilyId))
famIds <- famIds[famIds$Freq <= 2,]
combi$FamilyId[combi$FamilyId %in% famIds] <- 'Small'
combi$FamilyId <- factor(combi$FamilyId)

### split combi in train and test data
trainData <- combi[1:891,]
testData <- combi[892:1309,]

## Predict
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + 
               FamilySize + FamilyId, data = trainData, method = "class")
# new.fit <- prp(fit, snip = TRUE)$obj
# fancyRpartPlot(new.fit)
prediction = predict(fit, testData, type='class')
submit <- data.frame(PassengerId = testData$PassengerId, Survived = prediction)
write.csv(submit, file = 'myfirstdtree.csv', row.names = FALSE)
