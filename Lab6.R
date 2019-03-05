# Ensembles

# three different types of wines
# we will use ensemble learning to see how well we can classify them
wine <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"),
                 header = F)

View(wine)
str(wine)

names(wine) <- c(
  "Alcohol",
  "MalicAcid",
  "Ash",
  "Height",
  "Alcalinity",
  "Magnesium",
  "TotalPhenols",
  "Flavanoids",
  "NonflavanoidPhenols",
  "Proanthocyanins",
  "ColorIntensity",
  "Hue",
  "OD280OD315",
  "Proline"
)

str(wine)
wine$Alcohol <- factor(wine$Alcohol) # make it categorical
barplot(table(wine$Alcohol))

boxplot(wine[,c(2:14)])

# features not normalised

#check for missing data
sapply(wine, function(x){sum(is.na(x))})

#### KNN ####

winekNN <- wine

#normalise numeric data
winekNN[,c(2:14)] <- sapply(winekNN[,2:14], 
                            FUN=function(x){
                              return (x-min(x)/(max(x)-min(x)))
                            })

summary(winekNN)

## create stratified validation sample
set.seed(357)
library(caret)

#75/25 data partition
index <- createDataPartition(winekNN$Alcohol, p=0.75, list = FALSE)

trainkNN <- winekNN[index,]
validationkNN <- winekNN[-index,]


#see which features are useful
library(randomForest)
training <- wine[index,]
rf <- randomForest(Alcohol ~ .,data=training, importance=TRUE, ntree=100)

varImpPlotData <- varImpPlot(rf)

# take some of the features - try other methods
# mean decrease in accuracy variables

meanDecAcc <- varImpPlotData[,1]

#order descending - most important first
meanDecAcc <- meanDecAcc[order(-meanDecAcc)]

# make a vector of numbers
a <- c(1:length(meanDecAcc))

#take the odd numbers
a <- a %% 2 == 1 

# get the odd colNames
kNNfeatures <- names(meanDecAcc[a])
kNNfeatures

# tuning params
tuneParams <- trainControl(method = "CV",
                           number = 10,
                           savePredictions = "final")
knn <- train(trainkNN[,kNNfeatures],trainkNN$Alcohol, method = "knn",
             trControl = tuneParams, tuneLength = 10)

knn.pred <- predict(knn, newdata=validationkNN[ ,kNNfeatures])

confusionMatrix(knn.pred, validationkNN$Alcohol)

#### C5.0 ######

library(C50)
train <- wine[index,]
validation <- wine[-index,]

# take only the even numbered features
c50Features <- names(meanDecAcc[!a])
c50Features

#train
c50Tree <- train(train[,c50Features], train$Alcohol, method="C5.0",
                 trControl=tuneParams, tuneLength=3)

c50.pred <- predict(c50Tree, newdata=validation[,c50Features])
confusionMatrix(c50.pred, validation$Alcohol)

# knn - 70 %
# c5.0 = 90.7 %
# CART = 88.37 %

#combination == 0.907

##### CART ##########
meanDecGini <- varImpPlotData[, 2]
meanDecGini <- meanDecGini[order(-meanDecGini)]

b <- c(1:length(meanDecGini))
b <- b %% 2 ==1

cartFeatures <- names(meanDecGini[b])
cartFeatures

trainCART <- wine[index,]
validationCART <- wine[-index,]

rpartTree <- train(trainCART[,cartFeatures], trainCART$Alcohol,
                   method="rpart",trControl=tuneParams,
                   tuneLength=3)

rpart.pred <- predict(rpartTree, newdata= validation[, cartFeatures])
confusionMatrix(rpart.pred, validationCART$Alcohol)

# combining 
# we will average the prediction probabilities of each class,
# then select the class label with highest average probability

validation$pred_rpart_prob <- predict(object=rpartTree, validationCART[, cartFeatures],
                                      type="prob")
validation$pred_c50_prob <- predict(object=c50Tree, validation[, c50Features],
                                      type="prob")
validation$pred_knn_prob <- predict(object=knn, validationkNN[, kNNfeatures],
                                    type="prob")

#get the averages
validation$pred_avg <- (validation$pred_rpart_prob+validation$pred_c50_prob+validation$pred_knn_prob)/3

#get the max
validation$preds <- apply(validation$pred_avg, 1, FUN=function(x){which.max(x)})

validation$preds <- factor(validation$preds, levels=c(1:3), labels=c(1:3))

confusionMatrix(validation$Alcohol,validation$preds)

# knn - 70 %
# c5.0 = 90.7 %
# CART = 88.37 %

#combination == 0.907

########### STACKING ##########
train <- wine[index,]
train$pred_knn <- factor(knn$pred$pred[order(knn$pred$rowIndex)])
train$pred_c50 <- factor(c50Tree$pred$pred[order(c50Tree$pred$rowIndex)])
train$pred_cart <- factor(rpartTree$pred$pred[order(rpartTree$pred$rowIndex)])
library(gbm)
install.packages("gbm")
predictors <- c("pred_knn", "pred_c50", "pred_cart")
gbm <- train(train[, predictors], train$Alcohol, methods="gbm", trControl=tuneParams, tuneLength=3)
gbm
