## This file contains a pair of functions that solve the inverse of a matrix, avoiding the computation cost by 
## caching the inverse of a matrix rather than compute it repeatedly 

## makeCacheMatrix  creates a special "matrix" object that can cache its inverse
## The first function, makeVector creates a special "vector", which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the solve: where solve() function solves equation a %*% x = b for x, where b is a vector or matrix.
##get the value of the solve

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)  
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
