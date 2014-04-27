## A pair of functions that cache the inverse of a matrix so that the inverse
## is only recalculated when it has not been previously calculated. Otherwise
## it is retrieved from the cache.
##
## Usage example: 
## create the matrix: a <- makeCacheMatrix(matrix(1:4,2))
## solve the inverse: ainv <- cacheSolve(a)
##
## R Programming course on Coursera, 2014
## by Janne Simonen

## This function creates a special "matrix" object that can cache its inverse.
## It returns a list of functions for accessing the cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve should retrieve the inverse from 
## the cache. Usage: just like the usual solve()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
