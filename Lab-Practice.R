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

