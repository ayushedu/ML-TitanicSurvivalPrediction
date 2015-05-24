# Kaggle Titanic project

## set wd
setwd('/Users/snowleopard/Documents/workspace/kaggle/titanic')

## clear workspace
closeAllConnections();
rm(list=ls())

#### Part 1 - Loading data & basic analysis #####

## load data
trainData <- read.csv('data/train.csv', header = T, stringsAsFactors = FALSE)
testData <- read.csv('data/test.csv', header = T, stringsAsFactors = FALSE)

## visualize data
head(trainData)
plot(density(trainData$Age, na.rm=T))
plot(density(trainData$Fare, na.rm=T))

## Survival rate by sex
counts <- table(trainData$Survived, trainData$Sex)
barplot(counts, xlab='Gender', ylab = 'Number of people', main="Survived and deceased in male and female")
counts[2] / (counts[1] + counts[2])
counts[4] / (counts[3] + counts[4])

## Survival rate by passenger class
pclassSurvival <- table(trainData$Survived, trainData$Pclass)
barplot(pclassSurvival, xlab='Cabin class', ylab = 'Number of people', 
        main="Survived and deceased by cabin class")
pclassSurvival[2] / (pclassSurvival[1] + pclassSurvival[2])
pclassSurvival[4] / (pclassSurvival[3] + pclassSurvival[4])
pclassSurvival[6] / (pclassSurvival[5] + pclassSurvival[6])

#### Part 2 - cleaning data ####
## Remove unused cols
trainData <- trainData[-c(1,9:12)]

## Replace gender var with dummy var
trainData$Sex <- gsub('female', 1, trainData$Sex)
trainData$Sex <- gsub('male', 0, trainData$Sex)

## Infer missing age values
### get index of people with specified surname
masterVec <- grep('Master.', trainData$Name, fixed=T)
missVec <- grep('Miss.', trainData$Name, fixed=T)
mrsVec <- grep('Mrs.', trainData$Name, fixed=T)
mrVec <- grep('Mr.', trainData$Name, fixed=T)
drVec <- grep('Dr.', trainData$Name, fixed=T)

### Replace full name of individual with title
for (i in masterVec) {
  trainData$Name[i] = 'Master'
}
for (i in missVec) {
  trainData$Name[i] = 'Miss'
}
for (i in mrsVec) {
  trainData$Name[i] = 'Mrs'
}
for (i in mrVec) {
  trainData$Name[i] = 'Mr'
}
for (i in drVec) {
  trainData$Name[i] = 'Dr'
}

### calculate title group averages
masterAge <- round(mean(trainData$Age[trainData$Name == "Master"], na.rm = TRUE), digits = 2)
missAge <- round(mean(trainData$Age[trainData$Name == "Miss"], na.rm = TRUE), digits = 2)
mrsAge <- round(mean(trainData$Age[trainData$Name == "Mrs"], na.rm = TRUE), digits = 2)
mrAge <- round(mean(trainData$Age[trainData$Name == "Mr"], na.rm = TRUE), digits = 2)
drAge <- round(mean(trainData$Age[trainData$Name == "Dr"], na.rm = TRUE), digits = 2)

### replace NA in age with group average
for(i in 1:nrow(trainData)) {
  if(is.na(trainData[i,5])) {
    if(trainData$Name[i] == 'Master') {
      trainData$Age[i] = masterAge
    } else if(trainData$Name[i] == 'Miss') {
      trainData$Age[i] = missAge
    } else if(trainData$Name[i] == 'Mrs') {
      trainData$Age[i] = mrsAge
    } else if(trainData$Name[i] == 'Mr') {
      trainData$Age[i] = mrAge
    } else if(trainData$Name[i] == 'Dr') {
      trainData$Age[i] = drAge
    }
  }
}

## Create additional vars
### new variable child - age<12. Val 1 is child, 2 otherwise
trainData['Child'] <- NA
for(i in 1:nrow(trainData)) {
  if(trainData$Age[i] < 12) {
    trainData$Child[i] <- 1
  } else {
    trainData$Child[i] <- 2
  }
}

### new variable family - family size
trainData['Family'] = NA
for(i in 1:nrow(trainData)) {
  x = trainData$SibSp[i]
  y = trainData$Parch
  trainData$Family[i] = x + y + 1
}

### new variable mother
trainData['Mother'] = NA
for(i in 1:nrow(trainData)) {
  if(trainData$Name[i] == 'Mrs' & trainData$Parch[i] > 0) {
    trainData$Mother[i] = 1    
  } else {
    trainData$Mother[i] = 2
  }
}

#### Clean test data ####
PassengerId = testData[1]
testData = testData[-c(1, 8:11)]

testData$Sex = gsub("female", 1, testData$Sex)
testData$Sex = gsub("^male", 0, testData$Sex)

test_master_vector = grep("Master.",testData$Name)
test_miss_vector = grep("Miss.", testData$Name)
test_mrs_vector = grep("Mrs.", testData$Name)
test_mr_vector = grep("Mr.", testData$Name)
test_dr_vector = grep("Dr.", testData$Name)

for(i in test_master_vector) {
  testData[i, 2] = "Master"
}
for(i in test_miss_vector) {
  testData[i, 2] = "Miss"
}
for(i in test_mrs_vector) {
  testData[i, 2] = "Mrs"
}
for(i in test_mr_vector) {
  testData[i, 2] = "Mr"
}
for(i in test_dr_vector) {
  testData[i, 2] = "Dr"
}

test_master_age = round(mean(testData$Age[testData$Name == "Master"], na.rm = TRUE), digits = 2)
test_miss_age = round(mean(testData$Age[testData$Name == "Miss"], na.rm = TRUE), digits =2)
test_mrs_age = round(mean(testData$Age[testData$Name == "Mrs"], na.rm = TRUE), digits = 2)
test_mr_age = round(mean(testData$Age[testData$Name == "Mr"], na.rm = TRUE), digits = 2)
test_dr_age = round(mean(testData$Age[testData$Name == "Dr"], na.rm = TRUE), digits = 2)

for (i in 1:nrow(testData)) {
  if (is.na(testData[i,4])) {
    if (testData[i, 2] == "Master") {
      testData[i, 4] = test_master_age
    } else if (testData[i, 2] == "Miss") {
      testData[i, 4] = test_miss_age
    } else if (testData[i, 2] == "Mrs") {
      testData[i, 4] = test_mrs_age
    } else if (testData[i, 2] == "Mr") {
      testData[i, 4] = test_mr_age
    } else if (testData[i, 2] == "Dr") {
      testData[i, 4] = test_dr_age
    } else {
      print(paste("Uncaught title at: ", i, sep=""))
      print(paste("The title unrecognized was: ", testData[i,2], sep=""))
    }
  }
}

#We do a manual replacement here, because we weren't able to programmatically figure out the title.
#We figured out it was 89 because the above print statement should have warned us.
testData[89, 4] = test_miss_age

testData["Child"] = NA

for (i in 1:nrow(testData)) {
  if (testData[i, 4] <= 12) {
    testData[i, 7] = 1
  } else {
    testData[i, 7] = 1
  }
}

testData["Family"] = NA

for(i in 1:nrow(testData)) {
  testData[i, 8] = testData[i, 5] + testData[i, 6] + 1
}

testData["Mother"] = NA

for(i in 1:nrow(testData)) {
  if(testData[i, 2] == "Mrs" & testData[i, 6] > 0) {
    testData[i, 9] = 1
  } else {
    testData[i, 9] = 2
  }
}


#### Part 3 - training model ####
## Linear regression
train.glm = glm(Survived ~ Pclass + Sex + Age + Child + Sex*Pclass + Family + Mother,
                family=binomial, data = trainData)
summary(train.glm)

## predict
p.hats <- predict.glm(train.glm, newdata = testData, type="response")
survival = vector()
for(i in 1:length(p.hats)) {
  if(p.hats[i] > 0.5) {
    survival[i] = 1
  } else {
    survival[i] = 0
  }
}
