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
install.packages("e1071")
library("class")
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

