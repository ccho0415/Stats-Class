

####################################
######  Begin Document 
####################################


setwd("~/Desktop/Data")


## Slide 4 -------------------------------

diamonds         <- read.csv("diamonds.csv", as.is = T)
diamonds$cut     <- factor(diamonds$cut)
diamonds$color   <- factor(diamonds$color)
diamonds$clarity <- factor(diamonds$clarity)

## Slide 12 -------------------------------

table(diamonds$cut) 
names(table(diamonds$cut))

## Slide 13 -------------------------------

barplot(height = table(diamonds$cut), 
        names.arg = names(table(diamonds$cut)))


## Slide 14 -------------------------------

levels(diamonds$cut)
diamonds$cut <- factor(diamonds$cut, level = c("Fair", 
                                               "Good", "Very Good", "Premium", 
                                               "Ideal"))
levels(diamonds$cut)


## Slide 15 -------------------------------

barplot(height = table(diamonds$cut), 
        names.arg = names(table(diamonds$cut)))

## Slide 17 -------------------------------


hist(diamonds$carat, main = "Histogram of Carats", 
     xlab = "Carats")


## Slide 18 -------------------------------


hist(diamonds$carat[diamonds$carat < 3], breaks = 100, 
     main = "Histogram of Carats", xlab = "Carats")

## Slide 23 -------------------------------

boxplot(price ~ cut, data = diamonds, ylab = "Price", 
        xlab = "Cut")

## Slide 25 -------------------------------


plot(diamonds$carat, diamonds$price, xlab = "Carats", 
     ylab = "Price ($)")


## Slide 27
plot(diamonds$carat, diamonds$price, xlab = "Carats", 
     ylab = "Price ($)")


## Slide 30 -------------------------------

set.seed(1)

rows       <- dim(diamonds)[1]
small_diam <- diamonds[sample(1:rows, 1000), ]

## Slide 31 -------------------------------

plot(log(small_diam$carat), log(small_diam$price),
     col = small_diam$cut)
legend("bottomright", legend = levels(small_diam$cut), 
       fill = 1:length(levels(small_diam$cut)), cex = .5)


## Slide 33 -------------------------------


abline(8, 0, col = "orange", lty = 2)
lm1 <- lm(log(small_diam$price) ~ log(small_diam$carat))
abline(lm1)


## Slide 35 -------------------------------

cuts        <- levels(small_diam$cut)
col_counter <- 1

for (i in cuts) {
  this_cut    <- small_diam$cut == i
  this_data   <- small_diam[this_cut, ]
  this_lm     <- lm(log(this_data$price) 
                    ~ log(this_data$carat))
  abline(this_lm, col = col_counter)
  col_counter <- col_counter + 1
}


## Slide 38 -------------------------------

points(-0.4, 6.8,  pch = "*", col = "purple")



## Slide 40 -------------------------------

text(-0.4, 6.8 - .2, "New Diamond", cex = .5)



