########################## Naive Bayes ########################
titanicData <- read.csv("titanicCleaned.csv")

#look at data
str(titanicData)

# some numerical values that we need to discretize or recode so that naive bayes can work
table(titanicData$SibSp)

# convert sibsp to factor

titanicData$SibSp <- factor(titanicData$SibSp)

table(titanicData$Parch)
# convert Parch to factor

titanicData$Parch <- factor(titanicData$Parch)

titanicData$Pclass <- factor(titanicData$Pclass)

str(titanicData)

# Discretise Age and Fare
par(mfrow=c(1,2))
hist(titanicData$Fare, breaks=30)
hist(titanicData$Age)

# Use cut() to bin Fare
# cut handles 0.00 and integers badly, so add 1 to Fare values
titanicData$Fare <- titanicData$Fare + 1
titanicData$Fare <- as.integer(titanicData$Fare)

titanicData$FareBinned <- cut(titanicData$Fare, 
                              breaks= c(0,10,50,max(titanicData$Fare)),
                              labels = c("low","medium","high"))

str(titanicData)
titanicData$Fare
titanicData$FareBinned
table(titanicData$FareBinned,titanicData$Pclass)

# Use cut() to bin Age
titanicData$AgeBinned <- cut(titanicData$Age, breaks=c(0,10,20,30,40,50,60,70,max(titanicData$Age)),
                             labels=c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70+"))

# remove all numeric features.
titanicData$Age <- NULL
titanicData$Fare <- NULL
titanicData$Fsize <- NULL

# Train Naive Bayes model
library(e1071)
library(caret)

#create training and test sets
index <- sample(1:nrow(titanicData),nrow(titanicData)*.75,replace=F)

training <- titanicData[index,]
testing <- titanicData[-index,]

nb <- naiveBayes(Survived ~ ., data=training)

#predict using training NB model
nbPredict <- predict(nb, newdata=testing[, -1])

# look at confusion Matrix
confusionMatrix(testing$Survived, nbPredict, positive="Yes")

######################### C5.0 Decision Tree ###################
install.packages("C50")
library(C50)

# decision trees can use numerical values so let's import the original data
titanicC50 <- read.csv("titanicCleaned.csv")

index <- sample(1:nrow(titanicC50), nrow(titanicC50)*.75, replace=F)

training <- titanicC50[index,]
testing <- titanicC50[-index,]

cFifty <- C5.0(Survived ~ ., data=training)
summary(cFifty)

# predict using training model
cFiftyPredict <- predict(cFifty, newdata = testing[,-1])

confusionMatrix(cFiftyPredict,testing$Survived, positive = "Yes")
