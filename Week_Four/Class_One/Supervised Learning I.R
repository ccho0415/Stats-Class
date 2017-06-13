
## Set wd
#setwd("~/Desktop/Data")

## Advertising data 
data <- read.table("Advertising.csv")

## Congress approval data
data <- read.csv("Congress.csv")
plot(data[,2],data[,1],xlab="Date",ylab="Approval")


##### Write kNN reg funciton
kNN.Regression <- function(x,data=data,k=5) {
  
  # Define distances
  D <- sqrt((x-data[,2])^2)
  
  # Find k-closest time points to x
  K.closest.ratings <- order(D)[1:k]
  
  # Return the mean of the k-corresponding response values (approval ratings)
  return(mean(data[K.closest.ratings,1]))
  
}

# Plot
K=100
x <- seq(from=min(data[,2]),to=max(data[,2]),by=.05)
plot(data[,2],data[,1],xlab="Date",ylab="Approval",main=paste("k =",K))
lines(x,sapply(x,kNN.Regression,data=data,k=K),col="purple")


# About the order function 
c(1,12,2,10,5)
y <- order(c(1,12,2,10,5))
y
c(1,12,2,10,5)[y]
