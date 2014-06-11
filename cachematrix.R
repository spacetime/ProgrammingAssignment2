## cacheMatrix.R contains two methods makeCacheMatrix, to create an object that retains a cached state and cacheSolve, to update the cached inverse.

## makeCacheMatrix creates an object that can store and retrieve a matrix
## and its inverse

makeCacheMatrix <- function(x = matrix()) {
    xInv <- NULL
    set <- function(y) {
        x <<- y
        xInv <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) xInv <<- inv
    getinverse <- function() xInv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve checks for a cached inverse and returns it if found.
## If not, it calculates the inverse, caches it, and returns it
## Inverse of the matrix is taken using the 'solve' function

## Assumptions: x is the input matrix, x is a square invertible matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("Returning cached inverse")
        return(inv)
    }
    xmatrix <- x$get()
    inv <- solve(xmatrix)
    x$setinverse(inv)
    inv    
}
