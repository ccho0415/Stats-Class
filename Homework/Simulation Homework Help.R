

######################################
# Inverse Transform (Cauchy)
######################################


# Cauchy pdf
f <- function(x) {
  
  return(1/(pi*(1+x^2)))
  
}
x <- seq(-8,8,by=.1)
plot(x,f(x),type="l",col="purple")

# Cauchy inverse cdf

F_inverse <- function(x) {
  
  return(tan(pi*(x-1/2)))
  
}

# Simulate Cauchy from uniform
set.seed(0)
U <- runif(100)
Y <- F_inverse(U)
hist(Y,probability = T,breaks=30,ylim=c(0,.5))
lines(x,f(x),col="purple")



######################################
# Reject-Accept Algorithm (Simulate standard normal using g~Cauchy)
######################################


# Standard normal pdf
f <- function(x) {
  
  return((1/sqrt(2*pi))*exp(-x^2/2))
  
}

## Slide 67
x <- seq(-5, 5, length = 100)
plot(x, f(x), type="l", ylab="f(x)",ylim=c(0,.75),col="red")

# Let g(x) be a Cauchy.  
# Try different values of alpha so that e(x)>f(x).
# alpha =.4,.5,.6..  Choose .6
alpha=.6
e <- function(x) {
  
  return(1/(alpha*pi*(1+x^2)))
         
}
plot(x, f(x), type="l", ylab="f(x)",ylim=c(0,.75),col="red")
lines(x,e(x))


n.samps <- 10000   # number of samples desired
n       <- 0		     # counter for number samples accepted
samps   <- numeric(n.samps) # initialize the vector of output
while (n < n.samps) {
  y <- F_inverse(runif(1))    #random draw from g
  #y <- rcauchy(1)    #Can also use rcauchy directly
  u <- runif(1)
  if (u < f(y)/e(y)) {
    n        <- n + 1
    samps[n] <- y
  }
}
x <- seq(-5, 5, length = 100)
hist(samps, prob = T, ylab = "f(x)", xlab = "x",
     main = "Histogram of draws from Normal",ylim=c(0,.45))
lines(x, f(x), lty = 2)


######################################
# Estimate pi using MC Integration
######################################

# Define g(x)
g <- function(x) {
  
return(sqrt(1-x^2))
}
# Choose p(x) ~ Unif(-1,1)
# Compute 1/n*Sum g(X_i)/p(X_i) 

# MC Integration
n <- 1000000
X <- runif(n,-1,1)
mean(2*2*g(X))




