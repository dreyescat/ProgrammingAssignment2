## Put comments here that give an overall description of what your
## functions do

## Cache matrix allows store potentially time-consuming operations on a matrix
## for subsequents uses without having to recompute it again.

## Write a short comment describing this function

## Creates a "special" cache matrix object that allows storing the cached result
## of the last performed operation.
## set: Sets the argument matrix
## get: Gets the argument matrix
## setop: Sets the cached result of the operation
## getop: Gets the cached result of the operation
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setop <- function(result) m <<- result
  getop <- function() m
  list(set = set, get = get, setop = setop, getop = getop)
}


## Write a short comment describing this function

## Calculates the inverse matrix of a "special" matrix returned by
## makeCacheMatrix.
## If the operation has already been calculated it returns this
## cached result.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getop()
  if (!is.null(m)) {
    message('getting cached data')
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setop(m)
  m
}