# load clean data
set.seed(1337)
titanicData <- read.csv("titanicCleaned.csv")

#split 70/30 train test data
index <- sample(1:nrow(titanicData),nrow(titanicData)*.70, replace = FALSE)

training <- titanicData[index,]
testing <- titanicData[-index,]

install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
library(RColorBrewer)

regressionTree <- rpart(Survived ~ ., data=training, method = "class")

# using only a subset of the variables
tree <- rpart(Survived ~ Pclass + Fare + Title + Fsize, data=training, method = "class")

#plot trees
plot(regressionTree)
plot(tree)
text(tree)

summary(regressionTree)

# Better plots
#install.packages("rattle")
library(rattle)

fancyRpartPlot(regressionTree)

# evaluate the performance of the regression tree
# 1. predict
# 2. confusion matrix
# 3. see if you can use AUROC

rPartPrediction <- predict(regressionTree, testing, type="class")
library(caret)
# look at confusion Matrix
confusionMatrix(testing$Survived, rPartPrediction, positive="Yes")

#------------------------------------------
# AUROC

library(ROCR)
ROCRperf <- performance(rPartPrediction,"tpr","fpr")

plot(ROCRperf)

# area under the curve for ROC curve
auc <- performance(rPartPrediction,measure = "auc")
auc<- auc@y.values[[1]]
auc
#------------------------------------------


# change some of the parameter of the regression line

newRPart <- rpart(Survived ~ ., data=training, control=rpart.control(minsplit = 2, cp=0))
fancyRpartPlot(newRPart)

# Huge tree has overfit the data
# evaluate performance of Huge Tree
rPartPrediction <- predict(newRPart,testing, type="class")

confusionMatrix(rPartPrediction, testing$Survived, positive = "Yes")

# Performance has reduced slightly, but passing the training data in will show us how much overfitting has occured

# original "small" tree
rPartPrediction <- predict(regressionTree, training, type="class")
confusionMatrix(rPartPrediction, training$Survived, positive="Yes")

# evaluate huge tree on testing data
rPartPrediction <- predict(newRPart,testing, type="class")

confusionMatrix(rPartPrediction, testing$Survived, positive = "Yes")

# Try to prune the tree, and see how well it works.









############################## Random Forests #############################
install.packages("randomForest")
library(randomForest)
#rfNews() tell us new developments in Random Forests

forest <- randomForest(Survived ~ . , data = training, importance = TRUE, ntree=2000)
# importance = true allows us to look at the variable importance
# ntrees = 2000, could use a smaller numbder if dataset is large,
# There are a few other parameters we could try
# complexity of node = nodesize
# num rows sampled = sampsize

varImpPlot(forest)

#evaluate the performance of random forest
forestPrediction <- predict(forest,testing, type="class")
confusionMatrix(forestPrediction, testing$Survived, positive = "Yes")
# acc, sens, spec, kappa


########################################## conditional inference tree ###################

library(partykit)
cTree <- ctree(Survived ~ . , data=training)
print(cTree)



####################################### Tuning Trees ####################################
# control resampling of data
objControl <- trainControl(method="cv",number=10,returnResamp = "none",
                           summaryFunction = twoClassSummary,
                           classProbs = T)

objModel <- train(Survived~.,data=training, method="C5.0",
                  trControl=objControl,
                  metric="ROC",
                  preProcess = c("center","scale"))
summary(objModel)
print(objModel)
plot(objModel)
