rm(list=ls())

# Okay, I figured out what happened with the dataset. So:
# - gender_submission: This is example of submission data.
# - test: the testing data (passanger #892-1309). Compare the generated data set with this!
# - train: the training data (passanger #1-891). Train the data set with this!
# The info is from Kaggle website.
#
# The big question for the data: What kind of people is most likely to survive?
#
# The variables:
# PassengerId: Passanger ID
# Survived: dead or not lol. 0: dead, 1: survived. 
# Pclass: Ticket class. Divided into 1="1st", 2="2nd", 3="3rd"
# Sex
# Age
# SibSp: #of siblings/spouse of the person on the ship (ignoring mistresses & fiances).
# Parch: #of parents & children of the person aboard the ship.
# Ticket: ticket#
# Fare: Ticket price (some of this are pricey as heck!)
# Cabin: Cabin#
# Embarked: port of embarkation. Have C="Cherbourg", S="Southampton", Q="Queenstown"

trainData <- read.csv("train.csv")
testData <- read.csv("test.csv")
library(caret)
library(kernlab)
set.seed(123)

#Let's know the data set a bit
names(trainData)
summary(trainData)
hist(trainData$Pclass, col="blue")
hist(trainData$Survived, col="red")
hist(trainData$SibSp, col="green")
hist(trainData$Parch, col="orange")

#I've tried all the methods that is applicable to binary data. I got the best one from Gradial Boosting Machine (GBM)
trainData$Survived <- as.factor(trainData$Survived)
indT <- createDataPartition(y=trainData$Survived,p=0.6,list=FALSE)
trainingInit <- trainData[indT,]
testingInit  <- trainData[-indT,]
ModFit <-train(Survived~Pclass+Sex+Embarked+SibSp,data=trainingInit,method="gbm",verbose=FALSE)
#summary(ModFit$finalModel)
prediction <- predict(ModFit, testingInit)
confusionMatrix(prediction,testingInit$Survived)

#Apply the data to the real testing data
prediction_real <- predict(ModFit, testData)
dataFile <- cbind(testData[1], prediction_real)
colnames(dataFile)[2] <- "Survived"
write.csv(dataFile,"gender_submission_Saliutama.csv")
