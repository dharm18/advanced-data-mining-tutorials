# help
help.start()
?mean

# generating data
x = rnorm(50)
y = rnorm(x)

?rnorm

#normal plots
plot(x,y)
boxplot(x)
hist(x)

#delete
x = NULL
rm(y)

x<-c(10.4, 5.6, 3.1, 6.4, 21.7)
# this is a comment
# c(10.4, 5.6, 3.1, 6.4, 21.7) -> x
x
1/x
y <- 1/x
y
# + - * / ^ 
#log exp sin cos tan sqrt
# max min
# range 
# length(x)
# sum(x)
# prod(x)

# TRUE FALSE

temp <- x > 13
temp

# < <= > >= == !=
# c1 & c2
# c1 | c2
# !c1

'\\'
"\n"
"\t"
"\b"
?Quotes

c("test","R")
paste(c("X","Y"),1:10,sep="")
#is.na(x)
z <- c(1:5,NA)
is.na(z)
# z == NA ; anything with NA becomes NA
0/0 #NA
Inf - Inf # NaN
is.nan(x)

### Indexes, selecting and modifying subsets of a data set
x <- c(-5:-1, NA, NA, 1:3)

y <- x[!is.na(x)]
y
y <- x[!is.na(x) & x> 0]
y

y <- x[1:5]
y

y <- x[-(1:5)]
y

#example
money <- c(10:100000,-10,NA,15)
strangeAnswers <- money[(is.na(money)) & money<0]
normalPeople <- money[(!is.na(money)) & money>=0 & money < 200]
toffs <- money[money > 10000]

fruit <- c(5,10,1,20)
names(fruit) <- c("orange","banana","apple","peach")
lunch <- fruit[c("apple","orange")]
lunch

x[is.na(x)] <- 0
x
x <- c(-5:-1, NA, NA, 1:3)
x[is.na(x)] <- mean(x[!is.na(x)])
x

# interacting with dataframes
x <- c(1:3, 7, 8:10)
attributes(x)
class(x)

z <- 0:9
class(z)

digits <- as.character(z)
class(digits)

d <- as.integer(digits)
class(d)

### dataframe

name <- c("A","B","D")
DAD <- c(20,30,66)
DBA <- c(1,2,22)

gender <- as.factor(c("F", "M", "M"))
nationality <- as.factor(c("IRL", "UK", "IRL"))
age <- c(20, 21, 22)

student.df <- data.frame(name,age,gender,nationality,DAD,DBA)
attributes(student.df)

# indexing
student.df['gender']
student.df$gender
student.df$gender[2]
mean(student.df$DBA)

student.df$average <- (student.df$DBA + student.df$DAD)/2
student.df

str(student.df)

student.df$name <- as.character(student.df$name)
str(student.df)

student.df <- rbind(student.df, c("D",23,"M","UK",56,65))

student.df$nationality <- as.character(student.df$nationality)
student.df <- rbind(student.df, c("Pri",27,"F","INDIA",56,65))
student.df <- rbind(student.df, c("Nit",23,"M","UK",45,60))
student.df <- rbind(student.df, c("Har",23,"M","UK",89,65))
student.df <- rbind(student.df, c("Nim",18,"F","INDIA",65,62))
student.df <- rbind(student.df, c("Har",23,"M","IRL",89,65))
student.df <- rbind(student.df, c("Sodi",18,"M","INDIA",65,62))

student.df$nationality <- as.factor(student.df$nationality)
levels(student.df$nationality)
?tapply
student.df
tapply(student.df$DBA, student.df$nationality, mean)
str(student.df)
student.df$DBA <- as.numeric(student.df$DBA)
student.df$DAD <- as.numeric(student.df$DAD)
str(student.df)
averages <- tapply(student.df$DBA, student.df$nationality, mean)
averages

#inspect data
table(student.df$nationality)
# graphically
barplot(table(student.df$nationality), xlab = "Nationality", ylab = "Count")

# inbuilt dataset

data("mtcars")
str(mtcars)
summary(mtcars)
?mtcars

mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$am <- factor(mtcars$am, labels = c("Automatic","Manual"), levels=c(0,1))
str(mtcars)
summary(mtcars)

hist(mtcars$mpg, breaks=10)
boxplot(mtcars$hp)
barplot(table(mtcars$cyl))
?coplot

coplot(mpg ~ hp | cyl, data=mtcars
       , panel = panel.smooth, rows = 1)

#Are there more automatic or manual cars?
table(mtcars$am)

#Which car is heaviest, and which is fastest?
mtcars[mtcars$wt >= max(mtcars$wt),]
mtcars[mtcars$qsec == min(mtcars$qsec),]

#Do automatic or manual cars have on average a better mpg?
averages <- tapply(mtcars$mpg, mtcars$am, mean)
averages
#How many cars have above average hp?

meanHp <- mtcars[mtcars$hp > mean(mtcars$hp),]
dim(meanHp)
#Of the cars that have above average hp, how many have 6 cylinders?
morethan6 <- (meanHp[meanHp$cyl == 6,])
dim(morethan6)
#Of the cars that have above average hp, and 6 cylinders, how many are automatic
dim(mtcars[(mtcars$hp > mean(mtcars$hp) & mtcars$cyl == 6 & mtcars$am == 'Automatic'),])

#Make a boxplot of mpg split by no. of cylinders
boxplot(mpg ~ cyl, data=mtcars)
#8. In the above boxplot are there outliers?
#not really, one with low mpg and cyl 8
#  9. Take a random 50% sample of the dataset, and rerun questions above to see what changes
testsample <- mtcars[sample(1:nrow(mtcars),nrow(mtcars)/2, replace=FALSE),]
testsample
#10. Split the dataset in 2, one half containing only automatics, and one half containing only manual transmissions
testsampleAuto <- testsample[testsample$am == 'Automatic',]
testsampleAuto
testsampleManual <- testsample[testsample$am == 'Manual',]
testsampleManual
#11. For each half, plot mpg against hp using the plot function
plot(mpg ~ hp, data=testsampleAuto, main="MPG vs. HP for Auto")
plot(mpg ~ hp, data=testsampleManual, main="MPG vs. HP for Manual")
#12. Add an “abline” that “fits” a linear model between the two variables in the plots you just drew
lmAuto <- lm(mpg ~ hp, data = testsampleAuto)
plot(mpg ~ hp, data=testsampleAuto,main="MPG vs. HP for Auto")
abline(lmAuto)

lmManu <- lm(mpg ~ hp, data = testsampleManual)
plot(mpg ~ hp, data=testsampleManual,main="MPG vs. HP for Manual")
abline(lmManu)

##########################lab 3######################

# to see packages installed
library()

#load package
library(packageName)
#install.packages update.packages from internet

# t() transpose function

install.packages(c("ggplot2", "ggthemes", "scales", "dplyr", "mice", "randomForest"))

getwd()
titanicData <- read.csv("titanic.csv", header = T, na.strings = c(""), stringsAsFactors = T)
str(titanicData)
summary(titanicData)

titanicData$Survived <- as.factor(titanicData$Survived)
titanicData$Pclass <- as.factor(titanicData$Pclass)
str(titanicData)

?sapply

#Check all attributes for missing values
sapply(titanicData, function(x){sum(is.na(x))})

# geographically inspect missing values
install.packages("data.table")
install.packages("Amelia")
library(Amelia)

# visual rep of missing data
missmap(titanicData, main="Missing values vs observed")

### handling missing values
## delete if very low percentage, use distribution of data
dim(titanicData)
2/891*100  # very low in amount

#titanicData <- titanicData[!is.na(titanicData$Embarked),]
## check who has missing data
paste("PassengerId : ", titanicData[is.na(titanicData$Embarked), 1], "needs to be corrected")

install.packages("dplyr")
library(dplyr)

embarked <- titanicData %>% filter(PassengerId != 62 && PassengerId != 830)
embarked

# Get these three details (class, fare, and name) of the two passengers.
titanicData[is.na(titanicData$Embarked),c("Pclass","Fare","Name")]

#or 

embarkedNAs <- titanicData %>% filter(PassengerId == 62 | PassengerId == 830)
print(embarkedNAs[, c(3,10,4)])

#----------------------------------------

mean(embarked$Fare)
install.packages("scales")
library(ggplot2)
library(ggthemes)
library(scales)

# Use ggplot2 to visualize embarkment, passenger class, & median fare
ggplot(embarked, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), #<--- the price we know for one passenger
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()

titanicData$Embarked[c(62, 830)] <- 'C'

sum(is.na(titanicData$Embarked))

#### AGE

(177/891)*100

ImputedAgeMean <- titanicData$Age
ImputedAgeMean[is.na(ImputedAgeMean)] <- mean(ImputedAgeMean, na.rm = TRUE)
sum(is.na(ImputedAgeMean))

?par
par(mfrow=c(1,2))
hist(titanicData$Age)
hist(ImputedAgeMean)

#median
ImputedAgeMedian <- titanicData$Age
ImputedAgeMedian[is.na(ImputedAgeMedian)] <- median(ImputedAgeMedian, na.rm = TRUE)
sum(is.na(ImputedAgeMedian))
par(mfrow=c(1,2))
hist(titanicData$Age)
hist(ImputedAgeMedian)

# descriptive statistics of data
df <- data.frame(titanicData$Age, ImputedAgeMean, ImputedAgeMedian)
summary(df)
# mean and median preserved in each case

#apply sd
sapply(df, function(x) sd(x, na.rm=T))

#### filling missing values using linear model

idx_na <- is.na(titanicData$Age)
age_train <- titanicData[-idx_na,]
age_test <- titanicData[idx_na,]

ageModel <- lm(Age~Pclass + Survived + SibSp, data=age_train)
age_test$Age <- predict(ageModel, newdata = age_test)

ImputedAgeLM <- titanicData

ImputedAgeLM[ImputedAgeLM$PassengerId %in% age_test$PassengerId, "Age"] <- age_test$Age

sum(is.na(ImputedAgeLM))

ImputedAgeMean[is.na(ImputedAgeMean)] <- mean(ImputedAgeMean, na.rm = T)

par(mfrow=c(2,2))
hist(titanicData$Age, freq = F, main = 'Age: Original', col = 'red', ylim=c(0,0.04))
hist(ImputedAgeMean, freq=F, main='Age: Imputed Age Mean', col='blue', ylim=c(0,0.04))
hist(ImputedAgeLM$Age, freq=F, main='Age: Imputed Age LM', col='green', ylim=c(0,0.04))

# imputation using machine learning

library(mice)
mice_mod <- mice(titanicData[, !names(titanicData) %in%
                               c('PassengerId','Name','Ticket','Cabin','Survived')], method='rf')

mice_output <- complete(mice_mod)

#Plot all the results to compare
par(mfrow=c(2,2)) # allows us to put plots into a 2 x 2 grid -- makes it easier to compare
hist(titanicData$Age, freq=F, main='Age: Original', col='red', ylim=c(0,0.04))
hist(ImputedAgeMean, freq=F, main='Age: Imputed Age Mean', col='blue', ylim=c(0,0.04))
hist(ImputedAgeLM$Age, freq=F, main='Age: Imputed Age LM', col='green', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', col='orange', ylim=c(0,0.04))

df <- data.frame(titanicData$Age, ImputedAgeMean, ImputedAgeMedian, 
                 ImputedAgeLM$Age, mice_output$Age)
summary(df)
sapply(df, function(x) sd(x, na.rm=T))

### mice random forest worked very well

#Missing values - Cabin
titanicData <- titanicData[,-11]

#######################################################

par(mfrow=c(1,1))
# distribution of Survived
barplot(table(titanicData$Survived))
# distribution of sex
barplot(table(titanicData$Sex))

ggplot(titanicData, aes(x= Sex, fill = factor(Survived))) + 
  geom_bar(stat = 'count', position = 'dodge') +
  labs(x='Sex') +
  theme_few()

## fill missing values
titanicData$Age <- mice_output$Age

ggplot(titanicData, aes(Age, fill = factor(Survived))) +
  geom_histogram() +
  facet_grid(.~Sex) +
  theme_few()

# Try out different combinations of Survived with other explanatory variables.

ggplot(titanicData, aes(Pclass, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  facet_grid(.~Sex) +
  theme_few()

ggplot(titanicData, aes(Pclass, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  facet_grid(.~Embarked) +
  theme_few()

#first inspect Fare
hist(titanicData$Fare)

titanicData$binnedFare <- cut(titanicData$Fare, breaks = c(-1, 0, 10, 25, 50, 100, 600),
    labels = c("Free","Cheapest", "Cheaper", "Mid Range","Expensive", "Most Expensive"))

ggplot(titanicData, aes(binnedFare, fill=factor(Survived))) +
                          geom_bar(stat='count', position='dodge') +
                          facet_grid(.~Sex) +
                          theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Create a new factor called Child

titanicData$Child[titanicData$Age < 18] <- "Y"
titanicData$Child[titanicData$Age >= 18] <- "N"
titanicData$Child <- as.factor(titanicData$Child)

ggplot(titanicData, aes(Child, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  facet_grid(.~Sex) +
  theme_few()

#

length(unique(titanicData$Name))
# Grab title from passenger names
titanicData$Title <- gsub('(.*, )|(\\..*)', '', titanicData$Name)
# Show title counts by sex
table(titanicData$Sex, titanicData$Title)

# Titles with very low cell counts to be combined to "rare" level
# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don',
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

#Also reassign mlle, ms, and mme accordingly
titanicData$Title[titanicData$Title == 'Mlle'] <- 'Miss'
titanicData$Title[titanicData$Title == 'Ms'] <- 'Miss'
titanicData$Title[titanicData$Title == 'Mme'] <- 'Mrs'
titanicData$Title[titanicData$Title %in% rare_title] <- 'Rare Title'
# Show title counts by sex again
table(titanicData$Sex, titanicData$Title)

#engineer a surname attribute
titanicData$Name <- as.character(titanicData$Name)

titanicData$Surname <- sapply(titanicData$Name,
                              FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})

# Create a family size variable including the passenger themselves
titanicData$Fsize <- titanicData$SibSp + titanicData$Parch + 1

# Create a family variable
titanicData$Family <- paste(titanicData$Surname, titanicData$Fsize, sep='_')

# Make the plot below, to help us understand if Fsize has any relationship with survival
ggplot(titanicData, aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()

# discretized family size

titanicData$FsizeD[titanicData$Fsize == 1] <- 'singleton'
titanicData$FsizeD[titanicData$Fsize > 1 & titanicData$Fsize < 5] <- 'small'
titanicData$FsizeD[titanicData$Fsize > 4] <- 'large'
titanicData$FsizeD <- as.factor(titanicData$FsizeD)
table(titanicData$FsizeD)

# consider order
titanicData$FsizeO[titanicData$Fsize == 1] <- 'singleton'
titanicData$FsizeO[titanicData$Fsize < 5 & titanicData$Fsize > 1] <- 'small'
titanicData$FsizeO[titanicData$Fsize > 4] <- 'large'
titanicData$FsizeO <- factor(titanicData$FsizeO, ordered = TRUE,
                             levels = c("singleton", "small", "large"))
table(titanicData$FsizeO)

# Show family size by survival using a mosaic plot
mosaicplot(table(titanicData$FsizeD, titanicData$Survived),
           main='Family Size by Survival', shade=TRUE)


# Pridiction

str(titanicData)
summary(titanicData)
titanicData$Pclass <- as.factor(titanicData$Pclass)
titanicData$Survived <- as.factor(titanicData$Survived)
titanicData$Title <- as.factor(titanicData$Title)


index <- sample(1:nrow(titanicData), nrow(titanicData) * .75, replace=FALSE)
train <- titanicData[index, ]
test <- titanicData[-index, ]


library(randomForest)
rf_model <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD, data = train)

varImpPlot(rf_model)

survived <- test$Survived

#strip out the attributes that we don't want; this is strictly speaking
#needed, but it's useful to see how we could filter if we wanted to
wantedAttributes <- c('Pclass', 'Sex', 'Age', 'SibSp', 'Parch',
                      'Fare', 'Embarked', 'Title', 'FsizeD')

test <- test[, names(test) %in% wantedAttributes]
str(test)

#predict
forestPrediction <- predict(rf_model, test, type = "class")
forestMissclassificationRate <- mean(forestPrediction != survived)
print(paste('Accuracy randomForest: ',1-forestMissclassificationRate, ' %'))

#intution
Gender <- rep(0,nrow(test))
Gender[test$Sex == 'female'] <- 1
GenderMissclassificationRate <- mean(Gender != survived)
print(paste('Accuracy all women survive: ',1-GenderMissclassificationRate, ' %'))

# confusion matrix
table(survived, forestPrediction)

# intution based
table(survived, Gender)


#################################################################################################


library(htmltab)

url <- "https://en.wikipedia.org/wiki/Game_of_Thrones"
seasons <- htmltab(doc=url, which=2)

head(seasons, 7)

rownames(seasons) <- c(1:8)
head(seasons)
#seasons$Season <- gsub("","",seasons$Season)

criticResponses <- htmltab(doc=url, which=3)
head(criticResponses)

str(seasons)
rownames(criticResponses) <- c(1:7)

seasons$Season <- factor(seasons$Season, levels = c("Season 1", "Season 2",
                                                    "Season 3", "Season 4", "Season 5", "Season 6", "Season 7"), labels=c(1:7))
criticResponses$Season <- factor(criticResponses$Season, levels = c(1:7), labels = c(1:7))

GoTWikipedia <- merge(seasons, criticResponses, by="Season")
head(GoTWikipedia)

fakeSeasons <- c(8:100)

#if we were using the original representation of this field instead:
#fakeSeasons <- paste0("Season ", c(8:100))
fakeText <- rep("fake", length(fakeSeasons))

fake.df.seasons <- data.frame(fakeSeasons, fakeText, fakeText, fakeText, fakeText, fakeText)
names(fake.df.seasons) <- names(seasons)
fake.df.seasons$Ordered <- as.character(fake.df.seasons$Ordered)
fake.df.seasons$Filming <- as.character(fake.df.seasons$Filming)
fake.df.seasons$`First aired` <- as.character(fake.df.seasons$`First aired`)
fake.df.seasons$`Last aired` <- as.character(fake.df.seasons$`Last aired`)
fake.df.seasons$`Novel(s) adapted` <- as.character(fake.df.seasons$`Novel(s) adapted`)

head(fake.df.seasons)

fakeReviews <- c(101:200)
fakeScores1 <- round(runif(length(fakeReviews), min=1, max=100))
fakeScores2 <- round(runif(length(fakeReviews), min=1, max=100))
fake.df.critics <- data.frame(fakeReviews)
names(fake.df.critics) <- c("Season")
fake.df.critics$`Critical response >> Rotten Tomatoes` <- paste0(fakeScores1, "% (33 reviews)")
fake.df.critics$`Critical response >> Metacritic` <- paste0(fakeScores2, " (34 reviews)")

head(fake.df.critics)

seasons$Season <- as.numeric(seasons$Season)
criticResponses$Season <- as.numeric(criticResponses$Season)
#we need to relax the factors to be able to include the new "seasons"

seasons <- rbind(seasons, fake.df.seasons)
criticResponses <- rbind(criticResponses, fake.df.critics)
#rebuild factors
seasons$Season <- factor(seasons$Season, levels = c(1:100), labels = c(1:100))
criticResponses$Season <- as.factor(criticResponses$Season)

library(dplyr)
seasons <- sample_n(seasons, size = nrow(seasons))
criticResponses <- sample_n(criticResponses, size=nrow(criticResponses))
head(seasons)

head(criticResponses)

GoTWikipedia2 <- merge(seasons, criticResponses, by="Season")
str(GoTWikipedia2)

#####################################################################################################

