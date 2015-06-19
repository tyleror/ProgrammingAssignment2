## The functions calculate the inverse of a matrix, but first check if the inverse 
## of the specific matrix is already cached

## makeCacheMatrix contains a list of functions that allow you to "set" and "get"
## the value and inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This functions checks to see if the inverse of the matrix of interest is already
## cached. If it is, it will return the value from the cache.
##If not, it will calculate the inverse of the matrix, returning the value and
## storing it in the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    ##Checks cache from above function
    if(!is.null(m)) {
        message('getting cached data')
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
