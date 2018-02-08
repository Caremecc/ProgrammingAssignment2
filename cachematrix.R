## Assignment: Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there are may be some benefits
## to caching the inverse of a matrix rather than computing it repeatedly. The two 
## functions below are used to create the special "matrix" object and then cache its inverse



## 1. The function makeCacheMatrix below, creates a special "matrix" object which can cache
##    its inverse. 

makeCacheMatrix <- function(x = matrix()) {
          i <- NULL
          set <- function(y) {
                  x <<- y
                  i <<- NULL
          }
          get <- function() x
          setInverse <- function(Inverse) i <<- inverse
          getInverse <- function() i
          list(set = set, get = get,
               setInverse = setInverse,
               getInverse = getInverse)
}


## 2. The function below computes the inverse of the special "matrix" returned by the makeCacheMatrix 
##    above. If the inverse was already calculated (and the matrix has not changed), then the cachesolve
##    should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 'x'
          i <- x$getInverse()
          if(!is.null(i)) {
            message("getting cached data")
            return(i)
          }
          data <- x$get()
          i <- solve(data, ...)
          x$setInverse(i)
          i
}