setwd("D://Ms in Data Analytics//ADM//Labs//ADM-Labs")
mnist <- read.csv("MNISTTrain.csv", header = T)
mnist$label <- factor(mnist$label)
table(mnist$label)

columnsKeep <- names(which(colSums(mnist[, -1]) > 0))
mnist <- mnist[c("label",columnsKeep)]

library(caret)
library(e1071)
library(ggfortify)
install.packages("ggfortify")

set.seed(1337)
index <- createDataPartition(mnist$label, p=.25, list=FALSE)

pca <- prcomp(mnist[index, -1], scale. = F, center = F)
autoplot(pca, data=mnist[index, ], colour='label')

screeplot(pca, type = "lines", npcs=50)

var.pca <- pca$sdev ^ 2
x.var.pca <- var.pca/sum(var.pca)
cum.var.pca <- cumsum(x.var.pca)

plot(cum.var.pca[1:200], xlab="No. of principal components",
     ylab="Cummulative Proportion of variance explained",
     ylim=c(0,1), type="b")


performanceSVM <- function(model, test, isTest) {
  results <- predict(model, test)
  if (isTest) {
    cm <- confusionMatrix(results, mnist[-index, 1])
  } else {
    cm <- confusionMatrix(results, mnist[index, 1])
  }
  return(cm$overall[c(1:2)])
}

pcs <- 10
train <- as.matrix(mnist[index,-1]) %*% pca$rotation[,1:pcs]
test <- as.matrix(mnist[-index, -1]) %*% pca$rotation[,1:pcs]

model <- svm(train, mnist[index,1], kernel = "linear")
(performanceSVM(model, test, TRUE))


minPCs <- 2
maxPCs <- 100

results <- data.frame()
kernels <- c("linear", "polynomial", "radial", "sigmoid")

## load already trained models
getwd()
load(file="results.RData")
library(reshape)

plotResults <- function(resultsDF, kernel){
  r <- resultsDF[resultsDF$Kernel == kernel, ]
  
  r <- r[, -5]
  #remove the kernel feature
  
  r$RuntimeM <- r$RuntimeM / max(max(r$RumtimeM), max(r$RuntimeE))
  r$RuntimeE <- r$RuntimeE / max(max(r$RumtimeM), max(r$RuntimeE))
  
  #transform runtime values into [0,1] (aids visualisation)
  
  m <- melt(r, id="PCs")
  
  ggplot(m, aes(x = PCs, y=value, colour = variable)) + geom_line() +
    ggtitle(paste("Performance for the ", kernel, "kernel against no. of PCs"))
}

#linear kernel
plotResults(results, kernels[1])
#polynomial kernel
plotResults(results, kernels[2])
#radial kernel
plotResults(results, kernels[3])
#sigmoid kernel
plotResults(results, kernels[4])


r <- results[, c(5:7)]
m <- melt(r, id=c("PCs","Kernel"))
ggplot(m, aes(x = PCs, y=value, colour = Kernel)) + geom_line() + 
  ggtitle("Kernel runtime against no. of PCs") + ylab("Runtime (secs)")
