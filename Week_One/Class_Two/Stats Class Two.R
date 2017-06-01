library(pixmap)
setwd("~/Desktop/Stats_Class/Week_One/Class_Two")
casablanca_pic <- read.pnm("casablanca.pgm")
casablanca_pic
plot(casablanca_pic)
dim (casablanca_pic@grey)
casablanca_pic@grey[360, 100]
casablanca_pic@grey[180,10]
casablanca_pic@grey[15:70, 220:265] <- 1
plot(casablanca_pic)

#locator
plot(rnorm(100), rnorm(100))
locator(1)

Z <- matrix(rnorm(200), nrow=100)
var(Z)
sd(Z)
