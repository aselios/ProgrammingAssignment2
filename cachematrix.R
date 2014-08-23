## The folowing calculates the inverse of a Matrix and cache the results to 
## improve the repeatedly computations.

## The next function take an invertible matrix as parameter and return a list
## with the matrix with a cache to store the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}
 

## This function take a invertible matrix as parameters and return the inverse of 
## the matrix. To do that use an special matrix that include a chache where is stored 
## the inverse matrix if was previously computed, if not after compute that the
## result going to be caching.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)){
          message("getting cached data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv, ...)
        inv
}
