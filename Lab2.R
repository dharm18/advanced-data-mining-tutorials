irisData <- iris

#Normalize all columns except species
irisData[,-5] <- scale(irisData[,-5])

#70/30 split
set.seed(345)

index <- sample(1:nrow(irisData),nrow(irisData)*.70, replace = FALSE)

trainingIris <- irisData[index,]
testIris <- irisData[-index,]

#run kNN
#install.packages("caret","class")
install.packages("data.table")
install.packages("e1071")
library("class")
library("data.table")
library("caret")
library("e1071")

# k = 1 
kNNPred1 <- knn(trainingIris[,-5], testIris[, -5], trainingIris$Species, k=1, prob=T)

#confusion matrix
table(testIris$Species, kNNPred1)

#calculate overall accuracy
sum(kNNPred1 == testIris$Species) / length(testIris$Species) * 100

# k = 3
kNNPred3 <- knn(trainingIris[,-5], testIris[, -5], trainingIris$Species, k=3, prob=T)
#confusion matrix
table(testIris$Species, kNNPred3)
#calculate overall accuracy
sum(kNNPred3 == testIris$Species) / length(testIris$Species) * 100

#Easier confusion Matrix from "caret" package
confusionMatrix((kNNPred3), testIris$Species)


#Evaluate the change in accuracy with increasing values of K
knnTestPrediction <- list()
accuracy <- numeric()

for (k in 1:100){
  
  knnTestPrediction[[k]] <- knn(trainingIris[,-5],testIris[, -5], trainingIris$Species, k, prob=T)
  
  accuracy[k] <- sum(knnTestPrediction[[k]] == testIris$Species)/ length(testIris$Species)*100
}


#plot
plot(accuracy, type="b", col="blue", cex=1, pch=20,xlab="Number of neighbours (k)", ylab="Classification accuracy(%)", main="Accuracy vs K" )

abline(h=max(accuracy), col="grey")

paste("Max accuracy found is ",max(accuracy), "% at k = ",which(accuracy==max(accuracy)))


# Logistic regression

titanicData <- read.csv("titanicCleaned.csv")
#80/20 split
set.seed(1337)
index <- sample(1:nrow(titanicData),nrow(titanicData)*.80, replace = FALSE)

trainingTitanic <- titanicData[index,]
testTitanic <- titanicData[-index,]

#Fit a logistic regression model
logit <- glm(Survived ~ ., family = binomial(link = "logit"), data = trainingTitanic)
logit

# prediction
logit.prediction <- predict(logit, newdata = testTitanic, type = "response")

results.logit <- ifelse(logit.prediction > 0.5, "Yes", "No")

#confusion matrix
table(testTitanic$Survived, logit.prediction > 0.5)

# ROC
install.packages("ROCR")
library(ROCR)

ROCRPred <- prediction(logit.prediction, testTitanic$Survived)
ROCRperf <- performance(ROCRPred,"tpr","fpr")

plot(ROCRperf)

# area under the curve for ROC curve
auc <- performance(ROCRPred,measure = "auc")
auc<- auc@y.values[[1]]
auc


#----------------------------------------------------------

install.packages("mice")
library(mice)
titanicData <- read.csv("titanic.csv", header=T, na.strings=c(""), stringsAsFactors = T)
titanicData$Survived <- factor(titanicData$Survived, levels = c(0,1), labels = c("No", "Yes"))
titanicData$Pclass <- as.factor(titanicData$Pclass)
titanicData <- titanicData[, -c(1,11)] #remove feature 1 and 11
titanicData$Embarked[c(62, 830)] <- 'C' #result of Embarked imputation exercise

#use a random forest to impute missing age values
mice_mod <- mice(titanicData[, !names(titanicData) %in%
                               c('PassengerId','Name','Ticket','Cabin','Survived')], method='rf')
mice_output <- complete(mice_mod)
titanicData$Age <- mice_output$Age
#feature engineering: make a feature to represent a passenger is a child
titanicData$Child[titanicData$Age < 18] <- "Yes"
titanicData$Child[titanicData$Age >= 18] <- "No"
titanicData$Child <- factor(titanicData$Child)
#feature engineer a title feature
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don',
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
titanicData$Title <- gsub('(.*, )|(\\..*)', '', titanicData$Name)
titanicData$Title[titanicData$Title == 'Mlle'] <- 'Miss'
titanicData$Title[titanicData$Title == 'Ms'] <- 'Miss'
titanicData$Title[titanicData$Title == 'Mme'] <- 'Mrs'
titanicData$Title[titanicData$Title %in% rare_title] <- 'Rare Title'
titanicData$Title <- as.factor(titanicData$Title)
#feature engineer a few more things using the passenger name
titanicData$Name <- as.character(titanicData$Name)
titanicData$Surname <- sapply(titanicData$Name,
                              FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
titanicData$Fsize <- titanicData$SibSp + titanicData$Parch + 1
#remove features 3, 7, and 11
titanicData[3] <- NULL
titanicData[7] <- NULL
titanicData[11] <- NULL
# feature engineer a family size categorical variable
titanicData$FsizeD[titanicData$Fsize == 1] <- 'singleton'
titanicData$FsizeD[titanicData$Fsize < 5 & titanicData$Fsize > 1] <- 'small'
titanicData$FsizeD[titanicData$Fsize > 4] <- 'large'
titanicData$FsizeD <- as.factor(titanicData$FsizeD)

contrasts(titanicData$Sex)

contrasts(titanicData$Pclass)

#80/20 split
set.seed(345)

index <- sample(1:nrow(titanicData),nrow(titanicData)*.80, replace = FALSE)

training <- titanicData[index,]
test <- titanicData[-index,]

n <- sapply(titanicData, function(x) {is.numeric(x)})
n
numerics <-titanicData[, n]
summary(numerics)

normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }
numericsNormal <- normalize(numerics)
summary(numericsNormal)

titanicDataKNN <- titanicData[, !n]
titanicDataKNN <- cbind(titanicDataKNN, numericsNormal)

install.packages("dummies")
library(dummies)

tkNN <- dummy.data.frame(titanicDataKNN[, -1])
summary(tkNN)
Survived <- titanicDataKNN$Survived

index <- sample(1:nrow(tkNN),nrow(tkNN)*.80, replace = FALSE)


str(tkNN)
kNNTraining <- tkNN[index,]
kNNTesting <- tkNN[-index,]
survivedTrain <- Survived[index]
survivedTest <- Survived[-index]

k1 <- round(sqrt(dim(kNNTraining)[1])) #sqrt of number of instances
k2 <- round(sqrt(dim(kNNTraining)[2])) #sqrt of number of attributes
k3 <- 7 #a number between 3 and 10
kNNTraining[,-1]

library(class)
knn1 <- knn(train = kNNTraining, test = kNNTesting, cl = survivedTrain, k=k1)
knn2 <- knn(train = kNNTraining, test = kNNTesting, cl = survivedTrain, k=k2)
knn3 <- knn(train = kNNTraining, test = kNNTesting, cl = survivedTrain, k=k3)

dim(kNNTraining)
dim(kNNTesting)
dim(survivedTrain)

confusionMatrix(knn1, survivedTest)
confusionMatrix(knn2, survivedTest)
confusionMatrix(knn3, survivedTest)
