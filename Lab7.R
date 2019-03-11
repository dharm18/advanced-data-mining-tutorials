#### SVM ######

# non-linearly seperable case #
set.seed(1)

# get some random data
x <- matrix(rnorm(20*2), ncol = 2)
x

# generate labels
y <- c(rep(-1,10), rep(1,10))
y

x[y==1, ] = x[y ==1, ]+1
y ==1
#plot
plot(x, col=3-y, pch=19, cex=1.5)

# encode y as a factor to do
# classification

dat <- data.frame(x = x, y=as.factor(y))
library(e1071)

svmfit <- svm(y~. , data=dat, kernel="linear", cost=10, scale=FALSE)

plot(svmfit, dat)

# there are 7 support vectors
svmfit$index

summary(svmfit)

#what's the best value of c

tune.out <- tune(svm, y~., data=dat, kernel="linear", ranges = list(costs=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)

#best model
bestmod <- tune.out$best.model
summary(bestmod)

# see how well model does with the test set
# generate test data
xtest <- matrix(rnorm(20*2), ncol=2)
ytest <- sample(c(-1,1), 20, rep=TRUE)

xtest[ytest == 1,] = xtest[ytest==1,] + 1
testdat <- data.frame(x=xtest, y=as.factor(ytest))

ypred <- predict(bestmod, testdat)
table(predict=ypred, truth=testdat$y)


## non linear boundary
# generatet some nonlinear data
x <- matrix(rnorm(200*2), ncol = 2)
x[1:100,] = x[1:100,] + 2
x[101:150,]=x[101:150,] - 2

y <- c(rep(1,150), rep(2,50))

dat <- data.frame(x=x, y=as.factor(y))
plot(x, col=y, pch=19, cex=1.5)

#split into training and test 50/50
trainIndex <- sample(1:nrow(dat), nrow(dat)*0.5, replace = F)

svmfit <- svm(y~.,data=dat[trainIndex,], kernel="radial", gamma=1, cost=1)
plot(svmfit, dat[trainIndex, ])

# tune using cv
tune.out <- tune(svm, y~., data=dat[trainIndex,], kernel="radial",
                 ranges=list(cost=c(0.1,1,10,100,1000), 
                             gamma=c(0.5, 1, 2, 3, 4, 5)))

summary(tune.out)

bestmodel <- tune.out$best.model
summary(bestmodel)

# svm with multiple classes
# add third class

x <- rbind(x, matrix(rnorm(50*2), ncol = 2))

# add third class's label
y <- c(y, rep(0, 50))
x[y==0, 2] = x[y==0, 2] + 2
dat <- data.frame(x=x, y=as.factor(y))

par(mfrow=c(1,1))
plot(x, col=(y+1))

# fit svm to data
svmfit <- svm(y~., data=dat, kernel="radial", cost=10, gamma=1)
plot(svmfit, dat)
