## makeCacheMatrix is a function which creates a matrix containig functions to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of the 
## matrix created by the makeCacheMatrix function. Because
## this calculation is resource intenssive it will first
## check to determine if the inverse value has already
## been generated and return the cached value if it is 
## available

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
## Thanks to Len Greski for "Demystifying makeVector()" which explained a lot I
## could not make sense of.

