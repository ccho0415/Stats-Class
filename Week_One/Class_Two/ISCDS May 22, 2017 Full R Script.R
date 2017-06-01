
####################################
######  Begin Document 
####################################

#How to Run Code
#Either Highlight and hit Run (Top Right)
#Or Use Ctrl+Enter

## Slide 22 -------------------------------

x <- c(5, 29, 13, 87)
x

## Slide 22 -------------------------------

x <- 1:50
x


## Slide 23 -------------------------------

x <- 2
mode(x)
typeof(x)
y <- as.integer(3)
typeof(y)


## Slide 29 -------------------------------

z <- 1 - 2i
z
typeof(z)

## Slide 30 -------------------------------

name <- "Columbia University"
name
typeof(name)

## Slide 31 -------------------------------

a <- TRUE
b <- F
a
b
typeof(a)


### Check Yourself -------------------------------
## Slide 33 -------------------------------

3*TRUE # Logicals in arithmetic
mode(3*TRUE)
mode("147")


## Slide 35 -------------------------------


x <- c(2, pi, 1/2, 3^2)
x

y <- c("NYC", "Boston", "Philadelphia")
y

## Slide 36 -------------------------------

z <- 5:10
z
  
u <- rep(1, 18)
u


## Slide 37 -------------------------------

v <- c()

v[1] <- TRUE
v[2] <- TRUE
v[3] <- FALSE

v

## Slide 38 -------------------------------

vec1 <- rep(-27, 3)
vec1

vec2 <- c(vec1, c(-26, -25, -24))
vec2


## Slide 39  -------------------------------


mat <- matrix(1:9, nrow = 3, ncol = 3)
mat


## Slide 40  -------------------------------

new_mat <- matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)
new_mat

## Slide 41  -------------------------------

this_mat <- matrix(nrow = 2, ncol = 2)

this_mat[1,1] <- sqrt(27)
this_mat[1,2] <- round(sqrt(27), 3)
this_mat[2,1] <- exp(1)
this_mat[2,2] <- log(1)

this_mat

## Slide 42 -------------------------------


vec1 <- rep(0, 4)
vec2 <- c("We're", "making", "matrices", "!")

final_mat <- rbind(vec1, vec2)
final_mat


## Slide 43 -------------------------------

this_mat # Defined previously
colnames(this_mat) # Nothing there yet

## Slide 44 -------------------------------

colnames(this_mat) <- c("Column1", "Column2")
this_mat

## Slide 45 -------------------------------

vec <- c(1.75, TRUE, "abc")
vec
str(vec)

## Slide 46 -------------------------------

# What does the str() function do?

# Function help:
?str

# Fuzzy matching:
??"structure"

## Slide 48 -------------------------------

y <- c(27, -34, 19, 7, 61)

y[2]

y[3:5]

y[c(1, 4)]


## Slide 49 -------------------------------

y <- c(27, -34, 19, 7, 61)
y
y[c(1, 4)] <- 0
y

## Slide 50 -------------------------------

y <- c(27, -34, 19, 7, 61)
y
y[-c(1, 4)]
y <- y[-1]
y

## Slide 51 -------------------------------

mat <- matrix(1:8, ncol = 4)
mat
mat[, 2:3]

## Slide 52 -------------------------------

this_mat
this_mat[, "Column2"]
this_mat[, -1]

## Slide 54 -------------------------------

# Installing the "pixmap" package.
install.packages("pixmap")
library("pixmap")

## Slide 56 -------------------------------

# Set working directory
setwd("~/Desktop/Data")
casablanca_pic <- read.pnm("casablanca.pgm")
casablanca_pic

plot(casablanca_pic)

## Slide 58 -------------------------------

dim(casablanca_pic@grey)
casablanca_pic@grey[360, 100]
casablanca_pic@grey[180, 10]

# Note: locator

locator(1)

## Slide 59 -------------------------------

casablanca_pic@grey[15:70, 220:265] <- 1
plot(casablanca_pic)


## Slide 62 -------------------------------

z <- matrix(rep(1:9), nrow = 3)
colnames(z) <- c("First", "Second", "Third")
z

z[2:3, "Third"]
c(z[,-(2:3)], "abc")

## Slide 64 -------------------------------

z
rbind(z[1,], 1:3)


## Slide 71 -------------------------------

# Define covariate and response variable 
x <- c(49.3,59.3,68.3,48.1,57.61,78.1,76.1)
y <- c(1894,2050,2353,1838,1948,2528,2568) 

## Slide 72 -------------------------------

n <- length(x) # Sample size
n

max(x)
sd(x)


## Slide 73 -------------------------------

summary(x) # Summary statistics
summary(y)


## Slide 75 -------------------------------

u <- c(1,3,5)
v <- c(1,3,5)
v + 4 # Recycling
v + c(1,3) # Recycling
v + u

## Slide 76 -------------------------------

u <- c(1,3,5)
v <- c(1,3,5)

'+'(v,u)
'*'(v,u)


## Slide 78 -------------------------------

plot(x,y, xlab = "Body Mass", ylab = "Energy Expenditure")



## Slide 79 -------------------------------

# First, compute x and y deviations 
dev_x <- x - mean(x)
dev_y <- y - mean(y)

# Next, compute sum of squares of xy and xx
Sxy <- sum(dev_x * dev_y)
Sxx <- sum(dev_x * dev_x)



## Slide 80 -------------------------------


# Compute the estimated slope 
Sxy/Sxx

# Compute the estimated intercept 
mean(y) - (Sxy/Sxx) * mean(x)

## Slide 83 -------------------------------

# Define matrix A
A <- matrix(c(3,1,1,-2,1/2,1,1,-12,3), nrow = 3) 

# Define vector b
b <- c(-1, 2, 3) 

# Use the solve function 
solve(A, b) 

## Slide 84 -------------------------------


x <- c(1, 2, 0) # Define solution vector x
A %*% x         # Then check with matrix multiplication


## Slide 87 -------------------------------


# Define matrix A
A <- matrix(c(1, -2, -2, 4), nrow = 2, byrow = TRUE)

# Define a 2 by 2 identity matrix 
identity <- diag(2)
identity

## Slide 88 -------------------------------


# Check if 5 is an eigenvalue of A
det(A - 5*identity)

## Slide 91 -------------------------------

1 > 3
1 == 3
1 != 3


## Slide 92 -------------------------------

(1 > 3) & (4*5 == 20)
(1 > 3) | (4*5 == 20)


## Slide 91 -------------------------------

c(0,1,4) < 3
which(c(0,1,4) < 3)
which(c(TRUE, TRUE, FALSE))

## Slide 94 -------------------------------

c(0,1,4) >= c(1,1,3)
c("Cat","Dog") == "Dog"

## Slide 95 -------------------------------

w <- c(-3, 20, 9, 2)

w[w > 3] ### Extract elements of w greater than 3

### What's going on here?
w > 3
w[c(FALSE, TRUE, TRUE, FALSE)]


## Slide 96 -------------------------------

w <- c(-3, 20, 9, 2)

### Extract elements of w with squares between 3 and 10
w[w*w >= 3 & w*w <= 10]

w*w >= 3 ### What's going on here?
w*w <= 10
w*w >= 3 & w*w <= 10


## Slide 97 -------------------------------

w <- c(-1, 20, 9, 2)
v <- c(0, 17, 10, 1)

### Extract elements of w greater than elements from v
w[w > v]

### What's going on here?
w > v
w[c(FALSE, TRUE, FALSE, TRUE)]


## Slide 98 -------------------------------

M <- matrix(c(rep(4,5), 5:8), ncol=3, nrow=3)
M

### We can do element-wise comparisons with matrices too.
M > 5



## Slide 99 -------------------------------

M
M[,3] < 8
M[M[,3] < 8, ]

## Slide 100 -------------------------------

M

### Assign elements greater than 5 with zero 
M[M > 5] <- 0
M


### Check yourself -------------------------------
## Slide 101 -------------------------------


z <- matrix(c(1:3, TRUE, FALSE, TRUE, 9, 16, 25), nrow = 3)
colnames(z) <- c("First", "Second", "Third")
z



## Slide 102 -------------------------------
z
z[z[, "Second"], ]


## Slide 103 -------------------------------

z
z[, 1] != 1
z[(z[, 1] != 1), 3]


## Slide 104 -------------------------------

z[(z[, 1] != 1), 3]
z[(z[, 1] != 1), 3, drop = FALSE]

## Slide 106 -------------------------------

length(c(-1, 0, NA, 5))
length(c(-1, 0, NULL, 5))

## Slide 107 -------------------------------

t <- c(-1,0,NA,5)
mean(t)
mean(t, na.rm = TRUE)

### NA values are missing, but NULL values don't exist.
s <- c(-1, 0, NULL, 5)
mean(s)


## Slide 108 -------------------------------


# Define an empty vector
x <- NULL
# Fill in the vector
x[1] <- "Blue"
x[2] <- "Green"
x[3] <- "Red"
x

## Slide 112 -------------------------------

# Define covariate and response variable 
x <- c(49.3,59.3,68.3,48.1,57.61,78.1,76.1)
y <- c(1894,2050,2353,1838,1948,2528,2568) 

# Combine data into single matrix
data <- cbind(x, y)

# Summary values for x and y
sum_x <- summary(x)
sum_y <- summary(y)

# We computed Sxy and Sxx previously 
est_vals <- c(Sxy/Sxx, mean(y) - Sxy/Sxx*mean(x))


## Slide 113 -------------------------------

body_fat <- list(variable_data = data, 
                 summary_x = sum_x, summary_y = sum_y, 
                 LOBF_est = est_vals)


## Slide 115 -------------------------------

# Extract the first list element 
body_fat[[1]]

## Slide 116 -------------------------------

# Extract the Line of Best Fit estimates 
body_fat$LOBF_est

# Extract the summary of x 
body_fat[["summary_x"]]


