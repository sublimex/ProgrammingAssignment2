## The functions below are used to compute and cache the inverse of a given invertible square matrix.
## The value is cached in order to avoid unnecessary time-consuming computations.

## The first function creates a special matrix that can cache its inverse, it contains functions that:
## set the value of the matrix, get the value of the matrix, set the value of the inverse matrix and
## get the value of the inverse matrix. 
## The variable 'x' is the matrix to be inverted.

makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    
    set <- function(y) {     x <<- y    
                             i <<- NULL }
    
    get <- function() x
    
    setinverse <- function(inverse) i <<- inverse
    
    getinverse <- function() i
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
   

}


## This function calculates the inverse of the matrix of the special matrix created with makeCacheMatrix()
## Before calculating it checks if the inverse has already been calculated. If so, it gets the value from the cache and does not calculate it
## If it has not been calculated before, it calculates it, stores it in cache and returns the inverse matrix.
## The variable 'x' is the result from makeCacheMatrix


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    i <- x$getinverse()
    
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    
    i <- solve(data, ...)
    
    x$setinverse(i)
    
    i
    
    
}
