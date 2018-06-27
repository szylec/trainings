### Coursera "R programming" week 1

getwd()
setwd("/Users/slechwar/Google_Drive/04_Dev/Inne/R courses/Coursea")

rm(list = ls())

# lapply: Loop over a list and evaluate a function on each element
# sapply: Same as lapply but try to simplify the result
# vapply: "safer" version of sapply, since it allows to set output date types
# apply: Apply a function over the margins of an array
# tapply: Apply a function over subsets of a vector
# mapply: Multivariate version of lapply


# lapply
x <- list(a = 1:5, b = rnorm(10), c = rnorm(20,1))
lapply(x, mean)

x1 <- 1:4
lapply(x1, runif)

x1 <- 1:4
lapply(x1, runif, min = 0, max = 10) # rest are arguments of the runif function

x2 <- list(a = matrix(1:4, 2, 2), b = matrix(1:6, 3, 2)) # list of 2 matrices
lapply(x2, function(elt) elt[,1])

# apply - for matrix (rows and columns)
x1 <- matrix(rnorm(200), 20, 10)
apply(x1, 1, sum) # sum of each row - dimension 1
apply(x1, 2, mean) # mean of each column - dimension 2

x2 <- matrix(rnorm(200), 20, 10)
apply(x2, 1, quantile, probs = c(0.25, 0.75)) # quantiles of the rows of the matrix

a <- array(rnorm(2 * 2 * 10), c(2, 2, 10))
apply(a, c(1, 2), mean)
# equal to
rowMeans(a, dims = 2)

# mapply
list(rep(1,4), rep(2,3), rep(3,2), rep(4,1))
mapply(rep, 1:4, 4:1)

# tapply
x <- c(rnorm(10), runif(10), rnorm(10,1))
f <- gl(3,10)

tapply(x, f, mean)
tapply(x, f, mean, simplify = FALSE)

tapply(x, f, range) # range gives min and max

# split
x <- c(rnorm(10), runif(10), rnorm(10,1))
f <- gl(3,10)

split(x, f)

lapply(split(x, f), mean)

library(datasets)
head(airquality)

s <- split(airquality, airquality$Month)
#class(s)
lapply(s, function(x) colMeans(x[, c('Ozone', 'Solar.R', 'Wind')]))
sapply(s, function(x) colMeans(x[, c('Ozone', 'Solar.R', 'Wind')]))
sapply(s, function(x) colMeans(x[, c('Ozone', 'Solar.R', 'Wind')], na.rm = TRUE))

#
x <- rnorm(10)
f1 <- gl(2,5)
f2 <- gl(5,2)

interaction(f1, f2)

str(split(x, list(f1, f2)))
str(split(x, list(f1, f2), drop = TRUE))

######################
# Debuging
######################

# we have:
# message
# warning
# error

# always think of NAs

#### Tools
# traceback: prints out the function call stack after an error occurs; does nothing if there’s no error
# debug: flags a function for “debug” mode which allows you to step through execution of a function one line at a time
# browser: suspends the execution of a function wherever it is called and puts the function in debug mode
# trace: allows you to insert debugging code into a function a specific places
# recover: allows you to modify the error behavior so that you can browse the function call stack

# traceback
rm(list = ls())
mean(x)
traceback() #gives most recent error

lm(y ~ x)
traceback()

# debug
debug(lm)
lm(y ~ x)

# recover
options(error = recover)
read.csv("nosuchfile")


################################
## QUIZ
################################

library(datasets)
data(iris)
?iris
head(iris)

round(tapply(iris$Sepal.Length, iris$Species, mean))
apply(iris[, 1:4], 2, mean)
class(apply(iris[, 1:4], 2, mean))
rowMeans(iris[, 1:4])

##
data("mtcars")
?mtcars

mean(mtcars$mpg, mtcars$cyl) # N
with(mtcars, tapply(mpg, cyl, mean)) # Y +++++++++++ REMEMBER THAT ONE YOU BITCH !!!!!
split(mtcars, mtcars$cyl) # N
tapply(mtcars$mpg, mtcars$cyl, mean) # Y
lapply(mtcars, mean) # N
apply(mtcars, 2, mean) # N
sapply(split(mtcars$mpg, mtcars$cyl), mean) # Y
tapply(mtcars$cyl, mtcars$mpg, mean) # N
sapply(mtcars, cyl, mean) # N


hp_cyl <- tapply(mtcars$hp, mtcars$cyl, mean)
class(hp_cyl)

round(abs(hp_cyl[1] - hp_cyl[3]))

debug(ls)

################################
## ASSIGNMENT
################################

rm(list = ls())

## The following functions make invert of matrix.
## First function calculates special object created with the makeCacheMatrix function. 
## It first checks to see if the invert has already been done. 
## If so, it gets inverted matrix from the cache and skips the computation. 
## Otherwise, it maks the invert of the matrix and sets it via the setmean function.

makeCacheMatrix <- function(x = matrix()) {
  # initialize inverse
  c <- NULL 
  # set x in env
  set <- function(y) {
    x <<- y
    c <<- NULL
  }
  get <- function() x
  # set inverse to desired value and return the value
  setsolve <- function(solve) c <<- solve
  getsolve <- function() c
  
  list(set = set, get = get,
      setsolve = setsolve,
      getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
  # check if cached value existing
  c <- x$getsolve()
  if(!is.null(c)) {
    message("getting cached data")
    return(c)
  }
  # get the matrix
  data <- x$get()
  # make inverse
  c <- solve(data)
  # set the value of the inverse
  x$setsolve(c)
  # return
  c
}

# example usage
x <- matrix(1:4, 2, 2)
makeCacheMatrix(x)$getsolve()

x <- cacheSolve(makeCacheMatrix(x))
x





#############

makeCacheMatrix <- function(x = matrix()) {
  cachedInv <- NULL ## initialize inverse
  
  ## set x in parent env with the desired value, if inverse is already set, get rid of it!
  set <- function(userValue = matrix()) {
    x <<- userValue 
    cachedInv <<- NULL
  }
  
  get <- function() x
  
  ##set inverse variable in parent env to desired value and return the value as a convenience
  setInverse <- function(invVal) {
    cachedInv <<- invVal 
    return(cachedInv)
  }
  
  getInverse  <- function() cachedInv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## given the list variable from the first function, will first check to see if there's already a cached inverse and return
## otherwise will attempt to solve its inverse and set/return it

cacheSolve <- function(x=makeCacheMatrix(1:4, nrow=2, ncol=2), ...) { ##special matrix provided or create a test 2x2 matrix
  
  ## let's see if there's something there already
  calculatedInverse <- x$getInverse() 
  
  ##check if there's a cached value AND it's a matrix
  if(!is.null(calculatedInverse) && is.matrix(calculatedInverse)) { 
    message("We found cached data and saved valuable cpus!!!")
    return(calculatedInverse)
  }
  
  ## otherwise get the matrix
  matrixToSolve <- x$get()  
  
  ## try to solve the matrix and catch errors and warnings
  calculatedInverse <- tryCatch({ 
    solve(matrixToSolve)
  }, warning=function(w) {
    message("This may not be the result you're looking for")
    message(w)
  }, error=function(e) {
    message("Something went wrong solving your matrix")
    message(e)
    message("\n")
  })
  
  ## whatever the case, set the value of the inverse (NULL if something went wrong)
  message("Setting the value of inverse to:") 
  x$setInverse(calculatedInverse)
}

x = matrix(1:4, 2, 2)

makeCacheMatrix(x)

cacheSolve(makeCacheMatrix(x))













