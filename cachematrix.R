## this function set up matrix for inverse calulcation
## set the value of the Matrix
## get the value of the Matrix
## set the value of the Inverse
## get the value of the Inverse


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


## this fuction return inverse of matrix

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
