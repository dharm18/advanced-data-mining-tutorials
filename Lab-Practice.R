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
mtcars[mtcars$wt == max(mtcars$wt),]
mtcars[mtcars$qsec == max(mtcars$qsec),]

