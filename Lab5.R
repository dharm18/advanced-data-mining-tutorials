# k- means 

table(iris$Species)
head(iris)
# remove the class label
myIris <- iris[,c(1:4)]
head(myIris)

# scaling data as k-means require normalised ip
myIris <- sapply(myIris, FUN=function(x){ scale(x, scale = T, center = T)})

# run k-means with 3 clusters
res.1 <- kmeans(myIris, 3)

# view result
str(res.1)

# create dataframe to map what specis is in what cluster
df <- data.frame(cluster = res.1$cluster, species = iris$Species)

table(factor(df$cluster),df$species)

# new data that we only scaling, no center
myIris <- iris[,c(1:4)]

myIris <- sapply(myIris, FUN=function(x){scale(x,scale = T, center = F)})

# try k-means again
res.2 <- kmeans(myIris, 3)

str(res.2)

# map species to cluster
df <- data.frame(cluster = res.2$cluster, species = iris$Species)

table(factor(df$cluster),df$species)


# selecting k
install.packages("clusterSim")
library(clusterSim)
library(cluster)

results <- list()
tot.withinss <- c()
betweenss <- c()
silhouette <- c()

for(k in 2:10){
  results[[k]] <- kmeans(myIris, k)
  tot.withinss[k] <- results[[k]]$tot.withinss
  betweenss[k] <- results[[k]]$betweenss
  s <- silhouette(results[[k]]$cluster, daisy(myIris))
  silhouette[k] <- mean(s[,3])
}

#plots
par(mfrow=c(2,2))
plot(tot.withinss,xlab="k")
plot(betweenss, xlab="k")
plot(silhouette, xlab="k")

# mtcars
head(mtcars)
str(mtcars)
# clusters mtcars
# use k from 2 to 10
# make plots
# what's the optimum value of k
mtcars <- mtcars[,c(1:4)]

results <- list()
tot.withinss <- c()
betweenss <- c()
silhouette <- c()
mtcars <- sapply(mtcars, FUN=function(x){scale(x,scale = T, center = F)})

for(k in 2:10){
  results[[k]] <- kmeans(mtcars, k)
  tot.withinss[k] <- results[[k]]$tot.withinss
  betweenss[k] <- results[[k]]$betweenss
  s <- silhouette(results[[k]]$cluster, daisy(mtcars))
  silhouette[k] <- mean(s[,3])
}

#plots
par(mfrow=c(2,2))
plot(tot.withinss,xlab="k")
plot(betweenss, xlab="k")
plot(silhouette, xlab="k")

#### hierarichical clustering colud be better here
hc = hclust(dist(mtcars))
plot(hc)
install.packages("ape")
library(ape)
plot(as.phylo(hc),cex=.9, label.offset = 1)



# try k-medoids with full mtcars later on




d <- dist(myIris, method="euclidean")
fit <- hclust(d, method="ward.D")
plot(fit)

# cut tree into three clusters
groups <- cutree(fit, k=7)
#draw red border around the 3 clusters
rect.hclust(fit, k=7, border = "red")
