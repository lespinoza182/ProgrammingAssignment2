## The makeCacheMatrix function creates a special matrix object that can 
##cache its inverse. The cacheSolve function returns the special matrix
##computed on the function above. If the inverse of the matrix has already been
##calculated, the function retrieve the inverse matrix from the cache.

## This function creates a matrix object that cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) i <<- inverse
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function cachesolves and returns a matrix that is the inverse of x. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("Getting cached data of inverse matrix...")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
