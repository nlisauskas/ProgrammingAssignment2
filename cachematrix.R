## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function create a matrix that can cache its inverse through the use of 4 functions: set, get, getinverse, and setinverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## Write a short comment describing this function
## This function gets the inverse of the matrix returned by makeCacheMatrix. It looks for whether the inverse of the matrix has been calculated and will either just return the matrix or run the function to produce it if not.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
