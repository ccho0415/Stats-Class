

####################################
######  Begin Document 
####################################



## Slide 17 -------------------------------

setwd("~/Desktop/Data")
Grocery <- read.table("Kutner_6_9.txt", header=T)
head(Grocery)

# Construct design matrix
X <- cbind(rep(1,52), Grocery$X1, Grocery$X2, Grocery$X3)

## Slide 18 -------------------------------

beta_hat <- solve((t(X) %*% X)) %*% t(X) %*% Grocery$Y
round(t(beta_hat), 2)


## Slide 20 -------------------------------

lm0 <- lm(Y ~ X1 + X2 + X3, data = Grocery)
lm0


## Slide 22 -------------------------------

lm0 <- lm(Y ~ X1 + X2 + X3, data = Grocery)

residuals(lm0)[1:5] 

sum(residuals(lm0))
sum(fitted(lm0))
sum(Grocery$Y)
fitted(lm0)[1:5]

## Slide 23 -------------------------------

summary(lm0)

## Slide 26 -------------------------------

v1 <- c(3,-2,7)
v2 <- c(-6,4,-14)
2*v1+v2


## Slide 28 -------------------------------

v1 <- c(3,-2,7)
v2 <- c(-2,12,9)
b <- c(8,-16,5)

2*v1-v2


## Slide 29 -------------------------------

v1 <- c(3,-2,7)
v2 <- c(-2,12,9)
A <- cbind(v1,v2)
b <- c(8,-16,5)
c <- c(2,-1)

A%*%c == b



## Slide 30 -------------------------------

A <- cbind(c(3,-2,7),c(-2,12,9),c(8,-16,5))
qr(A)$rank

## Slides 31-33 -------------------------------

# Define X_4
X4 <- ifelse(Grocery$X3==1,0,1)

# Define redundant design matrix 
X <- cbind(rep(1,52), Grocery$X1, Grocery$X2, Grocery$X3,X4)
head(X)

# Rank, Determinant, Inverse 
qr(t(X) %*% X)$rank
det(t(X) %*% X)
solve((t(X) %*% X))

# Linear model
lm(Y~X1+X2+X3+X4,data=Grocery)

# Define design matrix 
X <- cbind(rep(1,52), Grocery$X1, Grocery$X2, Grocery$X3)
head(X)

# Rank, Determinant, Inverse 

qr(t(X) %*% X)$rank
det(t(X) %*% X)
#Solve function will not work because it isn't a full column rank
solve((t(X) %*% X))

# Linear model
lm(Y~X1+X2+X3+X4,data=Grocery)

lm(Y~X1+X2+X3,data=Grocery)
# It can invert it without X4 but it is missing a slope



