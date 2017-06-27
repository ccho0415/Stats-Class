
#########################
# Load in data set:
#########################

data <- read.table("Example71.txt")

#########################
# Plot of raw data
#########################
plot(data$x,data$y,xlab="Tumor size (cm), X", ylab="Lymph node metastasis, Y",main="Raw Data",ylim=c(-.2,1.2),xlim=c(0,12))
abline(h=0,lty=3)
abline(h=1,lty=3)
abline(v=min(data$x),lty=2,col=1)
abline(v=max(data$x),lty=2,col=1)

#########################
# Estimate logistic model usinf glm()
#########################

#If you run the glm (general linear models) function then you have to link it
model <- glm(y~x,data=data,family=binomial(link = "logit"))
#Size of the tumor is 7 cm how likely is it that it'll spread?
#Predict function always gives linear prediction (B0 + B1Xi)
linear.pred <- predict(model,newdata=data.frame(x=7))
probs <- exp(linear.pred)/(1+exp(linear.pred))
probs


#########################
# Plot of raw data and estimated model
#########################

plot(data$x,data$y,xlab="Tumor size (cm), X", ylab="Lymph node metastasis, Y",main="Raw Data",ylim=c(-.2,1.2),xlim=c(0,12))
abline(h=0,lty=3)
abline(h=1,lty=3)
abline(v=min(data$x),lty=2,col=1)
abline(v=max(data$x),lty=2,col=1)
x.pred <- seq(-1,13,by=.1)
linear.pred <- predict(model,newdata=data.frame(x=x.pred))
probs <- exp(linear.pred)/(1+exp(linear.pred))
lines(x.pred,probs,col="purple")


#########################
# Maximum likelihood estimation 
#########################

#########################
# Log-likelihood function (negative)
#########################

logistic.Neg.LL <- function(b,data=data) {
  
  b0 <- b[1]
  b1 <- b[2]
  x <- data$x
  y <- data$y
  #The corresponding probalities that the x matches y
  p.i <- exp(b0+b1*x)/(1+exp(b0+b1*x))
  #Use Negative so we can minimizing instead of maximizing
  return(-sum(dbinom(y,size=1,prob=p.i,log=TRUE)))
  
}

logistic.Neg.LL(b=c(0,0),data=data)
logistic.Neg.LL(b=c(-3,.6),data=data)


#########################
# grad.descent from class
#########################

library(numDeriv)

grad.descent <- function(f, x0, max.iter = 200, step.size = 0.05, stopping.deriv = 0.01, ...) {
  
  n    <- length(x0)
  xmat <- matrix(0, nrow = n, ncol = max.iter)
  xmat[,1] <- x0
  
  for (k in 2:max.iter) {
    # Calculate the gradient
    grad.cur <- grad(f, xmat[ ,k-1], ...) 
    
    # Should we stop?
    if (all(abs(grad.cur) < stopping.deriv)) {
      k <- k-1; break
    }
    
    # Move in the opposite direction of the grad
    xmat[ ,k] <- xmat[ ,k-1] - step.size * grad.cur
  }
  
  xmat <- xmat[ ,1:k] # Trim
  return(list(x = xmat[,k],
              xmat = xmat, 
              k = k, 
              minimum=f(xmat[,k],...)
  )
  )
}

Newton.Method <- function(f, x0, max.iter = 200, stopping.deriv = 0.01, ...){
  n    <- length(x0)
  xmat <- matrix(0, nrow = n, ncol = max.iter)
  xmat[,1] <- x0
  
  for (k in 2:max.iter) {
    # Calculate the gradient
    grad.cur <- grad(f, xmat[ ,k-1], ...) 
    
    #Calculate the hessian
    hess.cur <- hessian(f, xmat[ ,k-1], ... )
    
    # Should we stop?
    if (all(abs(grad.cur) < stopping.deriv)) {
      k <- k-1; break
    }
    
    # Move in the opposite direction of the grad
    #Use Inverse of hess.cur and matrix multiply with grad curve
    xmat[ ,k] <- xmat[ ,k-1] - solve(hess.cur)%*%grad.cur
  }
  
  xmat <- xmat[ ,1:k] # Trim
  return(list(x = xmat[,k],
              xmat = xmat, 
              k = k, 
              minimum=f(xmat[,k],...)
  )
  )  
}

#########################
# Optimization using grad.descent()
#########################

x0 <- c(0,0)
gd <- grad.descent(logistic.Neg.LL,x0,max.iter = 2000, step.size = 0.01, stopping.deriv = 0.001,data=data)
gd$minimum
gd$k
gd$x
coef(model)

x0 <- c(0,0)
nm <- grad.descent(Newton.Method,x0,max.iter = 2000, stopping.deriv = 0.001,data=data)
nm$minimum
nm$k
nm$x
coef(model)

#########################
# Optimization using nlm()
#########################


nml.opt <- nlm(logistic.Neg.LL,c(0,0),data=data)
nml.opt




