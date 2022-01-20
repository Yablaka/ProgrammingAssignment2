## These functions are used to cache the inverse of a matrix
## This is done to avoid unnecessary & costly repeated computations

## This function returns a list containing functions that, for a given matrix
## set the value of the matrix, get the value of the matrix, 
## set the inverse of the matrix and get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        invrs <- NULL
        set <- function(y) {
          x <<- y
          invrs <<- NULL
        }
        get <- function() x
        setinverse <- function(z) invrs <<- z
        getinverse <- function() invrs
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function returns the cached inverse of matrix if available
## otherwise, it calculates the inverse of the matrix and caches this
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invrse <- x$getinverse()
        if(!is.null(invrse)) {
          message("getting cached data")
          return(invrse)
        }
        data <- x$get()
        invrse <- solve(data, ...)
        x$setinverse(invrse)
        invrse
}
