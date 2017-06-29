


## Slide 24

# install.packages("ISLR")
library("ISLR")
head(USArrests)

USArrests <- apply(USArrests, 2, scale)
head(USArrests)

## Slide 26

pca <- prcomp(USArrests)
pca


## PCA Example

par(mfrow = c(1,1), mar = c(1, 1, 1, 1))
biplot(pca)
abline(h=0,lty=2,col="grey")
abline(v=0,lty=2,col="grey")


## Slide 45

set.seed(2)
# Create a random matrix where the first column is
# N(3, 1) and the 2nd column N(-4, 1)

x <- matrix(rnorm(50*2), ncol = 2)
x[1:25, 1] <- x[1:25, 1] + 3
x[1:25, 2] <- x[1:25, 2] - 4
head(x, 5)

## Slide 46


km.out <- kmeans(x, centers = 2, nstart = 20)

# Cluster assignments for the 50 observations

km.out$cluster[1:25]
km.out$cluster[26:50]

plot(x, col = (km.out$cluster + 1), 
     main = "K-Means Clustering Results with K=2", 
     xlab = "", ylab = "", pch = 20, cex = 2)

## Slide 48

set.seed(4)
km.out <- kmeans(x, centers = 3, nstart = 20)
km.out$cluster

## Slide 51

set.seed(3)
km.out <- kmeans(x, centers = 3, nstart = 1)
km.out$tot.withinss
km.out <- kmeans(x, centers = 3, nstart = 20)
km.out$tot.withinss

