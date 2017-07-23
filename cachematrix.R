## This file contains 2 functions that allow you to cache the inverse of a matrix
## and then when the inversa of the matrix is needed again verify you have a cached value
## and pull the cached value rather that recalcualting the inverse.

## This function takes an invertable matrix and creates a list of functions that will 
## enable the caching of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set <- function(y = matrix) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(z){ 
    inv <<- z
  }
  getinv <- function() inv
  list(set =set, get=get, setinv=setinv, getinv=getinv)
}

## This function uses the list of functions from makeCacheMatrix() to verify if the inverse
## of the matrix is cached and:
## If it is return the cached matrix
## If not calculate and return the inverse and set the cached value for future use

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  ## Check for cached inverse
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  ## Calculate inverse and set cached calue for next time
  mat <- x$get()
  inv <- solve(mat)
  x$setinv(inv)
  return(inv)
}
