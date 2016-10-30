## Name: cachematrix.R -- R Programming assignment 2
## Purpose: calculate the inverse matrix using cache mechanism
## Created: 10/30/2016

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setInv <- function(inverse) inv <<- inverse
    
    getInv <- function() inv
    
    list(set = set,
         get = get,
         setInv = setInv,
         getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated,
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInv()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setInv(i)
    i
}
