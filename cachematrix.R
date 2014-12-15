## Programming assignment 2 of R programming on Coursera
## A pratice of lexical scoping in R
##
## Usage:
##      > source("cachematrix.R")
##      > x <- c(2, 1, 5, 3)
##      > attr(x, "dim") <- c(2, 2)
##      > x
##           [,1] [,2]
##      [1,]    2    5
##      [2,]    1    3
##      > x_with_cache <- makeCacheMatrix(x)
##      > cacheSolve(x_with_cache)
##           [,1] [,2]
##      [1,]    3   -5
##      [1,]    3   -5
##      > cacheSolve(x_with_cache)
##      getting cached data
##           [,1] [,2]
##      [1,]    3   -5
##      [1,]    3   -5
##      > x %*% cacheSolve(x_with_cache)
##      getting cached data
##           [,1] [,2]
##      [1,]    1    0
##      [2,]    0    1
##
## Hou-Chun Chen, 2014/12/15

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL
    set <- function(y) {
        x <<- y
        x_inv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) x_inv <<- inv
    getinv <- function() x_inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cacheSolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
