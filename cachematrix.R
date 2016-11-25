
## The purpose of these functions is to cache a matrix inversion computation.
## Matrix is generally a time-consuming computation, and therefor we may
## find value in caching in inverse instead of computing it repeatedly.

## makeCacheMatrix() is a set of functions to
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the value of the inverse matrix
##      4. get the value of the inverse matrix
## The function takes in on argument, which should be a square matrix.
## We are assuming that the matrix passed into this function has an inverse.

# cacheSolve() is a function that:
##      1. checks to see if the inverse is already cached
##      2. returns the inverse if it is already cached
##      3. if not, compute the inverse and set the inversion in the cache
##      4. return the inverse
## The function takes in one argument, which should be the output of
## the makeCacheMatrix() function



## makeCacheMatrix creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinversion <- function(solve) m <<- solve
        getinversion <- function() m
        list(set = set,
             get = get,
             setinversion = setinversion,
             getinversion = getinversion)
}


## CacheSolve calculates an inverse matrix or retreives from cache if possible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinversion()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinversion(m)
        m
}
