# Kaggle Titanic project using RandomForest

## set wd
setwd('/Users/snowleopard/Documents/workspace/kaggle/titanic/randomForest')

## clear workspace
closeAllConnections();
rm(list=ls())

#### Part 1 - Loading data & basic analysis #####

## load data
trainData <- read.csv('../data/train.csv', header = T, stringsAsFactors = FALSE)
testData <- read.csv('../data/test.csv', header = T, stringsAsFactors = FALSE)

## combine data
testData$Survived <- NA
combi <- rbind(trainData, testData)
summary(combi$Age)

## Feature engineering
### new column title - mr, mrs, etc
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split = '[,.]')[[1]][2]})
combi$Title <- sub(' ','', combi$Title) # remove spaces before the title

#### comnbine common titles
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- as.factor(combi$Title)

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

### predict NA age values using decision trees
Agefit <-  rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, 
                 data=combi[!is.na(combi$Age),], method = "anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

### New variable deck
combi$Deck <- as.factor(substr(combi$Cabin,1,1))

### New variable ticketType
combi$TicketType <- combi$Ticket
combi$TicketType <- gsub('/','',combi$TicketType)
combi$TicketType <- gsub('\\.','',combi$TicketType)
combi$TicketType <- substr(combi$TicketType,1,2)
combi$TicketType <- gsub('[0-9][0-9]','other',combi$TicketType)
combi$TicketType <- as.factor(combi$TicketType)
### replace blank embarked field
combi$Embarked[c(62, 830)] = 'S'
combi$Embarked = factor(combi$Embarked)

### remove NA value in Fare
combi$Fare[1044] <- median(combi$Fare, na.rm=T)

### reduce levels for FamilyId
combi$FamilyId2 <- combi$FamilyId
combi$FamilyId2 <- as.character(combi$FamilyId2)
combi$FamilyId2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyId2 <- factor(combi$FamilyId2)

combi$Sex <- factor(combi$Sex)

### split combi in train and test data
trainData <- combi[1:891,]
testData <- combi[892:1309,]



## Random forest
library(randomForest)
library(party)
set.seed(415)
# fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked +
#                       Title + FamilySize + FamilyId2 + Deck + TicketType, data=trainData, importance = TRUE, 
#                     ntree = 2000)
# varImpPlot(fit)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked +
                      Title + FamilySize + FamilyId + Deck + TicketType, data=trainData, 
               controls=cforest_unbiased(ntree=2000, mtry=3))

Prediction <- predict(fit, testData, OOB = TRUE, type="response")
submit <- data.frame(PassengerId = testData$PassengerId, Survived = Prediction)
write.csv(submit, file='firstforest.csv', row.names=FALSE)

## 
