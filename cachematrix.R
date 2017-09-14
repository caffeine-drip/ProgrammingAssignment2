## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  setM <- function(newValue) {
    x <<- newValue
    cache <<- NULL
  }
  getM <- function() x
  cacheI <- function(c) cache <<- c
  getI <- function() cache
  list(setM = setM, getM = getM, cacheI = cacheI, getI = getI)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inverse <- x$getI()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$getM()
  inverse <- solve(data, ...)
  x$cacheI(inverse)
  
  inverse
}
