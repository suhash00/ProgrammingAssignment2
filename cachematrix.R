## This script consists of two functions makeCacheMatrix and cacheSolve that help compute the inverse of
## a matrix (the cacheSolve function) and caches it (the makeCacheMatrix function). When the inverse is requested 
## for a matrix whose inverse has been computed previously, the inverse will be returned from the cache
## instead of being recomputed

## This function creates a list containing functions to set the value of the matrix, get the value of the matrix
## set the value of the inverse of the matrix and get the value of the inverse. It also has two data objects
## the matrix itself and its cached inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) i <<- inverse 
  
  getInverse <- function() i
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## This function retuns the inverse of the matrix x. If the inverse exists in the cache, it returns the cached value
## and if the inverse is not present in the cache, it computes the inverse, caches the value and returns it.

cacheSolve <- function(x, ...) {
  
  i<- x$getInverse()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  mat <- x$get()
  i <- solve(mat, ...)
  x$setInverse(i)
  i      
}
