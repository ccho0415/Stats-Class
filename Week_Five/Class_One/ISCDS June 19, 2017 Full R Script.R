

## Slide 5
install.packages("MASS")
library(MASS)
head(cats)


## Slide 6

boxplot(cats$Hwt ~ cats$Sex, 
        main = "Male and Female Cat Heart Weights")

## Slide 7

plot(density(cats$Hwt[cats$Sex == "F"]), col = "red", 
     xlim = c(4, 18), main = "Male and Female Cat Heart Weights")
lines(density(cats$Hwt[cats$Sex == "M"]), col = "blue")

## Slide 8

girlcats <- cats$Sex == "F"
t.test(cats$Hwt[girlcats], cats$Hwt[!girlcats])



## Slide 14

girlcats <- cats$Sex == "F"
Dhat     <- mean(cats$Hwt[girlcats])-mean(cats$Hwt[!girlcats])
nf   <- sum(girlcats); nm <- sum(!girlcats)
P    <- 10000
sample_diffs <- rep(NA, P)

for (i in 1:P) {
  data <- cats$Hwt[sample(1:(nf+nm))]
  meanf <- mean(data[1:nf])
  meanm <- mean(data[-(1:nf)])
  sample_diffs[i] <- meanf-meanm
  
}

pval <- mean(abs(sample_diffs) >= abs(Dhat))
pval





## Slide 15

girlcats <- cats$Sex == "F"
Dhat     <- mean(cats$Hwt[girlcats])-mean(cats$Hwt[!girlcats])
nf   <- sum(girlcats); nm <- sum(!girlcats)
P    <- 10000
sample_diffs <- rep(NA, P)

for (i in 1:P) {
  
  perm_data <- cats$Hwt[sample(1:(nf+nm))]
  meanf     <- mean(perm_data[1:nf])
  meanm     <- mean(perm_data[-(1:nf)])
  sample_diffs[i] <- meanf - meanm
}

pval <- mean(abs(sample_diffs) >= abs(Dhat))
pval


## Slide 22

board = list(
  R1 = 6.35,  # center to double bullseye ring
  R2 = 15.9,  # center to single bullseye ring
  R3 = 99,    # center to inner triple ring
  R4 = 107,   # center to outer triple ring
  R5 = 162,   # center to inner double ring
  R = 170,    # center to outer double ring
  nums = c(20,1,18,4,13,6,10,15,2,17,3,19,
           7,16,8,11,14,9,12,5)) # numbers in order


## Slide 26
drawBoard <- function(values) { 
  
  R1 = values$R1
  R2 = values$R2
  R3 = values$R3
  R4 = values$R4
  R5 = values$R5
  R = values$R
  nums = values$nums
  
  par(mar = c(0, 0, 0, 0))
  plot(c(), c(), axes = FALSE, xlim = c(-R - 15, R + 15),ylim = c(-R - 15, R + 15))
  
  t = seq(0, 2 * pi, length = 5000)
  x = cos(t)
  y = sin(t)
  points(R * x, R * y, type = "l")
  points(R5 * x, R5 * y, type = "l")
  points(R4 * x, R4 * y, type = "l")
  points(R3 * x, R3 * y, type = "l")
  points(R2 * x, R2 * y, type = "l")
  points(R1 * x, R1 * y, type = "l")
  t0 = pi/2 + 2 * pi/40
  points(c(R2 * cos(t0), R * cos(t0)), c(R2 * sin(t0),R * sin(t0)), type = "l")
  for (i in 1:19) {
    t1 = t0 - i * 2 * pi/20
    points(c(R2 * cos(t1), R * cos(t1)), c(R2 * sin(t1),R * sin(t1)), type = "l")
  }
  
  r = R + 10
  for (i in 1:20) {
    t1 = pi/2 - (i - 1) * 2 * pi/20
    text(r * cos(t1), r * sin(t1), nums[i])
  }
}

drawBoard(board)

## Slide 27
## Note: I nested a loop.. maybe you can write a better function 


scorePositions <- function(x.points,y.points,values) {
  
  n <- length(x.points)
  R1 = values$R1
  R2 = values$R2
  R3 = values$R3
  R4 = values$R4
  R5 = values$R5
  R = values$R
  nums = values$nums
  
  score.list <- rep(0,n)
  
  for (k in 1:n) {
    
    x <- x.points[k]
    y <- y.points[k]
    
    radius <- sqrt(x^2+y^2)
    if (x>=0 & y>=0) {theta <- acos(x/radius)}
    if (x>=0 & y<0) {theta <- -acos(x/radius)}
    if (x<0 & y<0) {theta <- -acos(x/radius)}
    if (x<0 & y>=0 & acos(x/radius) <= pi/2 + 2 * pi/40) {theta <- acos(x/radius)}
    if (x<0 & y>=0 & acos(x/radius) > pi/2 + 2 * pi/40) {theta <- -(2*pi-acos(x/radius))}
    
    t0 = pi/2 + 2 * pi/40
    score.theta <- NULL
    for (i in 0:19) {
      score.theta[i+1] <- (theta > t0 - (i+1) * 2 * pi/20)&(theta <= t0 - i * 2 * pi/20)
    }
    
    if (radius <= 6.35) {
      score <- 50
    } else if (6.35 < radius & radius <= 15.9) {
      score <- 25
    } else if (99 < radius & radius <= 107) {
      score <-  3*nums[score.theta] 
    } else if (162 < radius & radius <= 170)  {
      score <- 2*nums[score.theta]
    } else if (radius > 170)  {score <- 0
    } else {score <- nums[score.theta]}
    
    score.list[k] <- score 
    
  }
  return(score.list)
}



## Slide 28
throws=100
x <- rnorm(throws,sd=50)
y <- rnorm(throws,sd=50)
scores <- scorePositions(x.point=x,y.point=y,values=board)

## Trial run with 100 throws

drawBoard(board)
points(x,y,col=2,pch=20)



## Slide 30

text(x, y + 8, scores, cex = .75)


###################################
# Check Yourself
###################################
## Slide 33

throws  <- 100 
R <- 170
x <- runif(throws, min = -R, max = R)
y <- runif(throws, min = -R, max = R)
drawBoard(board)
points(x, y, pch = 20, col = "red")


## 35


scores <- scorePositions(x, y, board)
text(x, y + 8, scores, cex = .75)

## Slide 37

throws <- 10000
std.dev <- 50
R <- 170
x1 <- rnorm(throws, sd = std.dev)
y1 <- rnorm(throws, sd = std.dev)
x2 <- runif(throws, min = -R, max = R)
y2 <- runif(throws, min = -R, max = R)

## Slide 38


scores1 <- scorePositions(x1, y1, board)
scores2 <- scorePositions(x2, y2, board)
mean(scores1)
mean(scores2)


###################################
# Check Yourself
###################################

## Slide 39

set.seed(1)

sd.values  <- seq(5, 150, length.out = 25)
n          <- length(sd.values)
avg.scores <- rep(NA, n)
names(avg.scores) <- round(sd.values,1)

for (i in 1:n) {
  x.throws <- rnorm(throws, sd = sd.values[i])
  y.throws <- rnorm(throws, sd = sd.values[i])
  scores   <- scorePositions(x.throws, y.throws, board) 
  avg.scores[i] <- mean(scores)
}
par(mar = c(1,1,1,1))
plot(sd.values, avg.scores, xlab = "Standard Deviation", 
     ylab = "Average Score")
abline(mean(scores2), 0, col = "red")




###################################
# Check Yourself
###################################

## Slide 43


drawBoard(board)
points(c(0, -32), c(103, -98), col = c("red", "blue"), 
       pch = 20, cex = 3)

## Slide 44

normal.score <- function(mean.x, mean.y, sd, board) {
  x1 <- rnorm(throws, mean = mean.x, sd = sd)
  y1 <- rnorm(throws, mean = mean.y, sd = sd)
  return(mean(scorePositions(x1, y1, board)))
}

std.dev <- 35

normal.score(0, 0, std.dev, board)
normal.score(0, 103, std.dev, board)
normal.score(-32, -98, std.dev, board)




## Slide 47


rnorm.reject <- function(n, mean = 0, sd = 1, 
                         min.val, max.val) {
  all.samps <- c()
  while (length(all.samps) < n) {
    samp <- rnorm(1, mean = mean, sd = sd)
    if (min.val < samp & samp < max.val) {
      all.samps <- c(all.samps, samp)
    }
  }
  return(all.samps)
}



## Slide 48
sampled.vals <- rnorm.reject(n = throws, sd = std.dev, 
                             min.val = -R, max.val = R)
summary(sampled.vals)


###################################
# Check Yourself
###################################
set.seed(1)


sd.values  <- seq(5, 150, length.out = 25)
n          <- length(sd.values)
avg.scores <- rep(NA, n)
names(avg.scores) <- round(sd.values,1)

for (i in 1:n) {
  x.throws <- rnorm.reject(throws, sd = sd.values[i], 
                           min.val = -R, max.val = R)
  y.throws <- rnorm.reject(throws, sd = sd.values[i], 
                           min.val = -R, max.val = R)
  scores   <- scorePositions(x.throws, y.throws, board) 
  avg.scores[i] <- mean(scores)
}
plot(sd.values, avg.scores, xlab = "Standard Deviation", 
     ylab = "Average Score")
abline(mean(scores2), 0, col = "red")




