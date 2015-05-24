## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  Inverse <- NULL
  setMatrix <- function(y) {
    x <<- y
    Inverse <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inv) Inverse <<- inv
  getInverse <- function() Inverse
  list(set = setMatrix, get = getMatrix,
       setInv = setInverse,
       getInv = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inverse <- x$getInv()
  if(!is.null(Inverse)) {
    message("getting cached data")
    return(Inverse)
  }
  data <- x$get()
  Inverse <- solve(data, ...)
  x$setInv(Inverse)
  Inverse
}
