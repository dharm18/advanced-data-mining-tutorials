titanicData <- read.csv("titanic.csv", header=T, na.strings = c(""), stringsAsFactors = T)
y <- titanicData$Survived
table(y)
y <- factor(y, levels=c(0,1), labels=c("No","Yes"))
table(y)

#explore the data
prop.table(table(y))
barplot(table(y), main = "Distribution of Titanic Survival", ylab="Frequency")
