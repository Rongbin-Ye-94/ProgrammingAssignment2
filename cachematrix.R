## Put comments here that give an overall description of what your
## functions do

## This function creates a special “matrix” object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  hidden <- NULL
  set <- function(y){
    x <<- y
    hidden <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) hidden <<- solveMatrix
  getInverse <- function() hidden
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## The function calculates the Inverse of the special “matrix” created with the above function.

cachemean <- function(x, ...) {
  hidden <- x$getInverse()
  if(!is.null(hidden)) {
    message("getting cached data")
    return(hidden)
  }
  data <- x$get()
  hidden <- solve(data, ...)
  x$setInverse(hidden)
  hidden
}
