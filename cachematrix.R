## Put comments here that give an overall description of what your
## functions do

##This program contains two functions, makeCacheMatrix and cacheSolve.
##makeCacheMatrix creates a special "matrix" object that can cache its inverse
##cacheSolve computes the inverse of the special "matrix" returned by
##makeCacheMatrix above


## Write a short comment describing this function

##makeCacheMatrix creates a special "matrix" object, and is a list that
##contains a function to:
##1) set the value of the matrix
##2) get the value of the matrix
##3) set the value of the inverse of the matrix
##4) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function

##cacheSolve first determines whether the inverse has been already calculated
##If yes, then it gets the value from the cache, and does not carry on with the
##computation
##If no, then it calculates the inverse of the matrix and stores the value in
##the cache using setInverse

cacheSolve <- function(x, ...) {
inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
        ## Return a matrix that is the inverse of 'x'
}
