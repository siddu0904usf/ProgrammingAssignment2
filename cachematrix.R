## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  Inverse <- NULL
  ##set the matrix
  setMatrix <- function(y) {
    x <<- y
    Inverse <<- NULL
  }
  getMatrix <- function() x ##get the matrix
  setInverse <- function(inv) Inverse <<- inv ##set the inverse of matrix
  getInverse <- function() Inverse ##get the inverse of the matrix
  ##the below list is used as input to the cacheSolve()
  list(set = setMatrix, get = getMatrix,
       setInv = setInverse,
       getInv = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inverse <- x$getInv() 
  ##get the inverse of the matrix if its already calculated
  if(!is.null(Inverse)) {
    message("getting cached data")
    return(Inverse)
  }
  ##Calculate the inverse of the matrix
  data <- x$get()
  Inverse <- solve(data, ...)
  x$setInv(Inverse)
  Inverse
}
