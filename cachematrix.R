## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function
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
