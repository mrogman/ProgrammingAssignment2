## Functions for solving for the inverse of a given matrix and caching the result

makeCacheMatrix <- function(x = matrix()) {
  ## Return list of functions for setting and getting the matrix and inverse of the matrix where
  ## 'x' is a square invertible matrix. The returned list is used as input for the cacheSolve() method.
  inverse = NULL
  set = function(y) {
    x <<- y
    inverse <<- NULL
  }
  get = function() x
  setinverse = function(inv) inverse <<- inv
  getinverse = function() inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
  ## returns the inverse of the original matrix where 'x' is the output of makeCacheMatrix()
  inverse = x$getinverse()
  if (!is.null(inverse)){
    return(inverse)
  }
  original = x$get()
  inverse = solve(original, ...)
  x$setinverse(inverse)
  return(inverse)
}
