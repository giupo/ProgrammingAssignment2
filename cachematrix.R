## This file contains functions to create a special matrix able to 
## store its inverse and cache it. 
##
## Matrix inverse is evaluated via `solve`; 
##
## We assume that the matrix is always invertible
 

#' Creates a special matrix which caches its own inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## gets the cached `solve` of `x` if already evaluated before; otherwise
## computes and caches it.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  } 
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
