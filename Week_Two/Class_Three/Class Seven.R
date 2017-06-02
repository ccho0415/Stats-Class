#Finding random sampling here
n <- 100
data < runif(n,min=0,max=10)
data
hist(data)
#Repeating 1000 times
R <- 1000
samp.dist <-rep(0,R)
#Find the Max for each Repeat
for(i in 1:R){
  data <-runif(n,min=0,max=10)
  samp.dist[i] <- mean(data)
}
#Sample distribution of the Maximum
head(samp.dist)
hist(samp.dist)

set.seed(0)
n <- 100
mu <- 2
data <-rnorm(n, mean= mu, sd=1)


B <- 1000
mean.boot <-rep(0,B)

for (b in 1:B){
  
  data.boot <- sample(data,n,replace=TRUE)
  mean.boot[b]  <- mean(data.boot)
  
}
hist(mean.boot)
var(mean.boot)

#The variance is close to true variance 1/100 (0.01)


set.seed(0)
n <- 200
x <-runif(200, 0, 10)
y <- 2-3.5*x+rnorm(n,sd=5)
plot(x, y)

data <-data.frame(x,y)

B <- 1000

slope.boot <-rep(0,B)

for (b in 1:B){
  #For bootstrapping a data frame you have to do it by pairs (x and y pair) find the indexes
  index.boot <- sample(1:n,n,replace=TRUE)
  # In order to index our rows in the data set we set the new indexes as rows
  data.boot <- data[index.boot, ]
  model.boot <- lm(y~x,data=data.boot)
  slope.boot[b]  <- coef(model.boot)[2]
  
}
hist(slope.boot)