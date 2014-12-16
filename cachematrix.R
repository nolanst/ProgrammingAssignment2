## This file enables the user to save time by calculating the inverse
## of a matrix and cache the result for quick, look-up reference 
## rather than calculate the value from scratch every time it is needed. 
## Function comments are adapted from documentation provided to class in 
## https://class.coursera.org/rprog-016/human_grading/view/courses/973757/assessments/3/submissions

## The first function, makeCacheMatrix, creates a special "matrix", 
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## The following function calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see if 
## the inverse has already been calculated. If so, it gets the inverse 
## from the cache and skips the computation. Otherwise, it calculates the 
## mean of the data and sets the value of the inverse in the cache via 
## the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}