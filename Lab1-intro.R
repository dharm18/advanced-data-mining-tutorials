titanicData <- read.csv("titanic.csv", header=T, na.strings = c(""), stringsAsFactors = T)
y <- titanicData$Survived
table(y)
y <- factor(y, levels=c(0,1), labels=c("No","Yes"))
table(y)

#explore the data
prop.table(table(y))
barplot(table(y), main = "Distribution of Titanic Survival", ylab="Frequency")

set.seed(1337)
index <- sample(1:length(y), length(y) * .25, replace = F)
testing <- y[index]

perishModel <- rep("No", length(testing))

coinModel <- round(runif(length(testing), min=0, max=1))
coinModel <- factor(coinModel, levels = c(0,1), labels = c("No", "Yes"))

perishModel <- factor(perishModel, levels = c("No", "Yes"), labels = c("No", "Yes"))

table(testing, perishModel)

table(testing, coinModel)

# Determining Prediction Accuracy

(coinAccuracy <- 1 - mean(coinModel != testing))
(perishAccuracy <- 1 - mean(perishModel != testing))
