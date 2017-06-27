

## Slide 6

library(MASS)
head(cats)

## Slide 7

hist(cats$Hwt)


## Slide 8

quantile(cats$Hwt, c(0.25, 0.5, 0.75))


## Slide 10

plot(ecdf(cats$Hwt), 
     main = "Empirical CDF of Cat Heart Weights")

## Slide 12

hist(cats$Hwt, probability = TRUE, ylim = c(0, 0.17))
lines(density(cats$Hwt), lty = "dashed")

# Check Yourself
## Slide 17

gamma.MMest <- function(data) {
  m <- mean(data)
  v <- var(data)
  return(c(a = m^2/v, s = v/m))
}

## Slide 18

gamma.MMest(cats$Hwt)


## Slide 19 

hist(cats$Hwt, probability = TRUE, ylim = c(0, 0.17))
lines(density(cats$Hwt), lty = "dashed")
cat.MM <- gamma.MMest(cats$Hwt) 
curve(dgamma(x, shape = cat.MM["a"], scale = cat.MM["s"]), 
      add = TRUE, col = "blue")

## Slide 20

gamma.mean <- function(a, s) {return(a*s)}
gamma.var  <- function(a, s) {return(a*s^2)}
gamma.diff <- function(params, data) {
  a <- params[1]
  s <- params[2]
  return((mean(data) - gamma.mean(a,s))^2 
         + (var(data) - gamma.var(a,s))^2)
}

## Slide 21

nlm(gamma.diff, c(19, 1), data = cats$Hwt)[1:3]


## Slide 24

gamma.MMest(rgamma(100, shape = 19, scale = 45))
gamma.MMest(rgamma(10000, shape = 19, scale = 45))
gamma.MMest(rgamma(1000000, shape = 19, scale = 45))

## Slide 28

gamma.ll <- function(params, data) {
  a <- params[1]
  s <- params[2]
  return(sum(dgamma(data, shape = a, 
                    scale = s, log = TRUE)))
}
gamma.ll(c(19, 0.05), cats$Hwt)

## Slide 29

nlm(gamma.ll, c(19, 1), data = cats$Hwt)[1:3]


## Slide 30

neg.gamma.ll <- function(params, data) {
  a <- params[1]
  s <- params[2]
  return(-sum(dgamma(data, shape = a,
                     scale = s, log = TRUE)))
}

nlm(neg.gamma.ll, c(19, 1), data = cats$Hwt)$minimum

nlm(neg.gamma.ll, c(19, 1), data = cats$Hwt)$estimate

cat.MM <- gamma.MMest(cats$Hwt) 
neg.gamma.ll(cat.MM, cats$Hwt)

## Slide 31

hist(cats$Hwt, probability = TRUE, ylim = c(0, 0.17))
lines(density(cats$Hwt), lty = "dashed")
cat.MLE <- nlm(neg.gamma.ll, c(19, 1), data = cats$Hwt)$estimate
curve(dgamma(x, shape = cat.MM["a"], scale = cat.MM["s"]), 
      add = TRUE, col = "blue")
curve(dgamma(x, shape = cat.MLE[1], scale = cat.MLE[2]), 
      add = TRUE, col = "red")

## Slide 35

# Model quantiles
qgamma(c(0.01, 0.05, 0.95, 0.99), shape = cat.MM["a"], 
       scale = cat.MM["s"])

# Data quantiles:
quantile(cats$Hwt, c(0.01, 0.05, 0.95, 0.99))

## Slide 37

a <- cat.MM["a"]; s <- cat.MM["s"]
qqplot(cats$Hwt, qgamma((1:99)/100, shape = a, scale = s), 
       ylab = "Theoretical Quantiles")
abline(0, 1, col = "red")

## Slide 39

plot(ecdf(pgamma(cats$Hwt, shape = a, scale = s)),
     main = "Calibration of gamma distribution for cat hearts")
abline(0, 1, col = "red")

## Slide 41

ks.test(cats$Hwt, pgamma, shape = a, scale = s)


## Slide 43 

n      <- length(cats$Hwt)
train  <- sample(1:n, size = round(.9*n))
cat.MM <- gamma.MMest(cats$Hwt[train])
a <- cat.MM["a"]
s <- cat.MM["s"]
a
s

## Slide 44

ks.test(cats$Hwt[-train], pgamma, shape = a, scale = s)


## Slide 45

ks.test(cats$Hwt[cats$Sex == "F"], 
        cats$Hwt[cats$Sex == "M"])


#QQ Line doesn't perfect match all the time.
z<- rnorm(50)
qqnorm(z)
qqline(z)
