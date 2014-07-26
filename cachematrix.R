## Pair of functions that permit the caching of the solution of a matrix.

## The makeCacheMatrix function encapsulates a matrix with a data store so that the
## solution of a matrix can be stored to save calculation time the next go round.
##
## @param   theMatrix  An R matrix
##
## @return  list of functions set, get, setInverse and getInverse
makeCacheMatrix <- function(theMatrix = matrix()) {
  cachedInverse <- NULL

  set <- function(newMatrix) {
    theMatrix <<- newMatrix

    ## When we set the matrix to a new value, we reset the cache
    cachedInverse <- NULL
  }

  get <- function() theMatrix

  setInverse <- function(inverse) cachedInverse <<- inverse

  getInverse <- function() cachedInverse

  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve takes a cached matrix value and computes the inverse of the matrix if it
## has not already been calculated.
##
## @param   theMatrix  A CacheMatrix value created by the makeCacheMatrix function
##
## @return  An R matrix that is the inverse of the original matrix.
cacheSolve <- function(theMatrix, ...) {
  inverse <- theMatrix$getInverse()

  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }

  nativeMatrix <- theMatrix$get()
  inverse <- solve(nativeMatrix)
  theMatrix$setInverse(inverse)
  inverse
}
