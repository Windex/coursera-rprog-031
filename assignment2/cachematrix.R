## Assignment for Coursera rprog-031
#   A pair of functions that allow for caching the inverse of a matrix
#   to avoid repeated costly calculations.

## Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    # Initialize the cache as NULL
    cinv <- NULL
    
    # Define get/set functions for matrix and cache inverse
    set <- function(y) {
        x <<- y
        cinv <<- NULL # clear any previously stored cached data
    }
    get <- function()
        x
    setCacheInv <- function(inv)
        cinv <<- inv
    getCacheInv <- function()
        cinv
    
    # Return list of functions
    list(
        set = set, get = get, setCacheInv = setCacheInv, getCacheInv = getCacheInv
    )
}


## Compute the inverse of a special "matrix" returned from makeCacheMatrix()
#   or return the cache for subsequent calls if the matrix has not changed

cacheSolve <- function(x, ...) {
    # Check if the computed inverse has already been cached and return that
    inv <- x$getCacheInv()
    if (!is.null(inv)) {
        message("getting cached data")
        return (inv)
    }
    
    # The cache value is NULL, so compute the inverse, cache and return it
    dat <- x$get()
    inv <- solve(dat)
    x$setCacheInv(inv)
    inv
}
