

install.packages("ggplot2")

####################################
######  Begin Document 
####################################


setwd("~/Desktop/Data")


## Slide 7 -------------------------------

diamonds         <- read.csv("diamonds.csv", as.is = T)
diamonds$cut     <- factor(diamonds$cut)
diamonds$color   <- factor(diamonds$color)
diamonds$clarity <- factor(diamonds$clarity)

set.seed(1)
rows <- dim(diamonds)[1]
diam <- diamonds[sample(1:rows, 1000), ]

## Slide 8 -------------------------------

plot(log(diam$carat), log(diam$price), col = diam$cut)
legend("bottomright", legend = levels(diam$cut), 
       fill = 1:length(levels(diam$cut)), cex = .5)

## Slide 9 -------------------------------

cuts        <- levels(diam$cut)
col_counter <- 1

for (i in cuts) {
  this_cut    <- diam$cut == i
  this_data   <- diam[this_cut, ]
  this_lm     <- lm(log(this_data$price) 
                    ~ log(this_data$carat))
  abline(this_lm, col = col_counter)
  col_counter <- col_counter + 1
}


## Slide 12 -------------------------------

points(-0.4, 6.8,  pch = "*", col = "purple")



## Slide 13 -------------------------------

text(-0.4, 6.8 - .2, "New Diamond", cex = .5)


## Slide 31 -------------------------------

library(ggplot2)

dim(mpg)
head(mpg, 3)

ggplot(data =mpg) +
  geom_point(mapping = aes (x= hwy, y =cyl) )
## Slide 32 -------------------------------

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

## Slide 38 -------------------------------

ggplot(data = mpg) + 
  geom_point(mapping = aes(x=displ, y=hwy, color=class))

## Slide 41 -------------------------------

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + facet_wrap(~ class, nrow = 2)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + facet_grid(drv ~ class)

## Slide 45 -------------------------------

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))

## Slide 48 -------------------------------

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))


## Slide 49 -------------------------------


ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(x=3, y=30), color = "purple") +
  geom_text(mapping = aes(x=3, y=31, label = "New Point"), size=4) +
  labs(title = "New Plot", x = "Engine Weight", y = "Highway mpg")

## Slide 50 -------------------------------

ggplot(data = diamonds) + 
  geom_point(mapping = aes(x = carat, y = price),alpha = 1/10)


## Some other examples -------------------------------

## Bar plot

head(mpg)
ggplot(data = mpg) +
  geom_bar(mapping=aes(x=class))+
  xlab("Car class") + ylab("Frequency") +
  ggtitle("Bargraph")

####################################
## Titanic data
####################################


titanic <- read.csv("Titanic.txt", header = TRUE, as.is = TRUE)
names(titanic)
head(titanic)

ggplot(aes(x=as.factor(Survived)), data=titanic) +
  geom_bar()+
  facet_grid(Sex~Pclass)

# Or 
ggplot(data=titanic) +
  geom_bar(aes(x=as.factor(Survived)))+
  facet_grid(Sex~Pclass)


