## The first function, makeVector creates a special "matrix", 
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
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the mean of the special "matrix" 
## created with the above function. However, it first checks to see if
## the inverse has already been calculated. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the inverse 
## of the data and sets the value of the mean in the cache via the setinverse
## function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


## Test Case 1

TestMatrix <- matrix(c(2,3,5,1,3,7,4,5,6,8,0,0,4,5,6,0),4,4)
TestMatrix
CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$get()
CacheMatrix$getinverse()
cacheSolve(CacheMatrix)
cacheSolve(CacheMatrix)

## Test Case 2

TestMatrix <- matrix(1:4,2,2)
CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$get()
CacheMatrix$getinverse()
cacheSolve(CacheMatrix)
cacheSolve(CacheMatrix)