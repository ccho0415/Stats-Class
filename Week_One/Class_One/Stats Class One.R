


# Simple math
2+4
# How do you run the code?
# Highlight the Code and hit Run (top right corner)

# How to store a value into a variable
# This is stored in the global environment
# Saving the workspace will save the global environments
a <- 3

a*4


this_mat <- matrix(nrow = 2, ncol = 2)
this_mat[1,1] <- sqrt(27)
this_mat[1,2] <- round(sqrt(27),3)
this_mat

#[] is notation on which part / square of the matrix we are assigning the number to

?mean

# A Way to install packages
install.packages("pixmap")
#Cannot use package until you use library function
library("pixmap")

#Call the Working Data
setwd("~/Desktop/Stats_Class/Week_One/Class_One")
casablanca_pic <- read.pnm("casablanca.pgm")
casablanca_pic
