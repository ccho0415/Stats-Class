#Cauchey Demo (It should be bouncing around to differen splots everytime you plot it)
f<- function(x){
  return(1/(pi*(1+x^2)))
}

x <-seq(-8,8, by=.1)
plot(x,f(x), type = "1", col = "purple")

F.inv <- function(x){
  return(tan(pi*(x-1/2)))
}

U <- runif(100)
Y <- F.inv(U)
head(Y)
hist(Y, breaks = 50)

hist(rcauchy(100))

f<- function(x){
  return((1/sqrt(2*pi))*exp(-x^2/2))
}

#Changing the Alpha chages where the envelope function ends up
alpha <- .5

#This is the envelope function
e <- function(x){
  return(1/(alpha*pi*(1+x^2)))
}

x<- seq(-5,5, by= 0.1)

plot(x,f(x), type = "l", col = "red", ylim = c( 0, .7))
lines(x,e(x))


n.samps <- 1000
n <- 0
samps <- numeric(n.samps)
while(n<n.samps){
  y <- rcauchy(1)
  u <- runif(1)
  if(u < f(y)/e(y)){
    n <- n+1
    samps[n] <- y
    }
}
x<- seq(-5, 5, length = 100)
hist(samps, prob = T, ylab = "f(x)", xlab = "x", main = "Histogram of draws from N(0,1)")
lines(x,f(x), lty = 2)

#Monte Carlo Demo
n <- 100000
#Vector of Random Draws from -1 to 1
X <- runif(n, -1, 1)
hist(X)
#Using Formula in slide 72
mean(4*sqrt(1-X^2))
