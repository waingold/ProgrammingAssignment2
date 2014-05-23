## The following functions support the creation & use of a special matrix 
## object that caches its own inverse.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    setMatrix <- function(value) {
        x <<- value
        inverse <<- NULL
    }
    getMatrix <- function() x
    setInverse <- function(value) inverse <<- value
    getInverse <- function() inverse
    list(setMatrix = setMatrix, getMatrix = getMatrix, 
         setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" object returned 
## by makeCacheMatrix above.  If the inverse has already been computed (and the
## underlying matrix hasn't changed), then it will return a cached copy.
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$getMatrix()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
