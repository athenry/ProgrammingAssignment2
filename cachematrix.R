## makeCacheMatrix and cacheSolve are a pair of functions to cache and retrieve the inverse
## of a supplied matrix.

## First, makeCacheMatrix takes an invertible matrix and returns a list of functions to 
## set the matrix, get the matrix, set the inverse, and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set=set, get=get,
             setinv=setinv,
             getinv=getinv)
}

## Given the output from makeCacheMatrix, casheSolve returns the inverse of the original matrix. 
## It first checks to see if the inverse has already been calculated: 
## if so, it retrieves the cached data; 
## if not, it solves for the inverse of the given matrix.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
