iris <- data.frame(iris)
#Possibility One
sum(iris$Species == "versicolor" & iris$Petal.Width <= 1.2)
#Possiblity Two
nrow(iris[iris$Species =="versicolor" & iris$Petal.Width <= 1.2])

#Filtering
mean(iris[iris$Species == "setosa", "Petal.Length"])
#TApply
tapply(iris$Petal.Length, iris$Species, mean)[1]

#Double && Does not work???

#Filtering with table function
table(iris[iris$Sepal.Width>=3,"Species"])

levels(iris$Species)
versicolor <- ifelse(iris$Species == "versicolor", 1, 0)
iris$versicolor <- versicolor
iris

hist(iris$Sepal.Width, xlab = "Sepal Width" , ylab = "Number of Flowers")

versifactor<- factor(versicolor)
plot(iris$Sepal.Length, iris$Sepal.Width, col = versifactor, xlab = "Sepal Length", ylab = "Sepal Width", main ="Comparing Sepal Length and Width")
legend("bottomright",legend=levels(versifactor), pch=c(1,1), col = c(1,length(levels(versifactor))), cex = 1)

irisfactor <- factor(iris$Species)

boxplot(iris$Petal.Length ~ iris$Species)
?boxplot

print("Columbia\tUniversity")
cat("Columbia\tUniversity")

lumvector <- c("Columbia", "slumber party", "sugarplum")

substr(lumvector, start = c(3,2,7), stop = c(5,4,10))
