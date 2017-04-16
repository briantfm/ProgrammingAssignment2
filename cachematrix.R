## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list that calls 4 functions: set, get, set_inverse
## and get_inverse. set and get are functions that stores and returns the
## matrix when called. In a similar fashion, set_inverse and get_inverse
## stores and loads the inverse. makeCacheMatrix then returns this list.
## Note: set_inverse does not calculate the inverse, this is left to cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    
    set_inverse <- function(calc_inv) inverse <<- calc_inv
    
    get_inverse <- function() inverse
    
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## cacheSolve calculates the inverse for the matrix stored in the list created
## by makeCacheMatrix, and stores the inverse in the list. It then returns the
## inverse.
## Note: Only need to call cacheSolve to manipulate the list, there's no need
## to use the built in functions in makeCacheMatrix to do this (as they're
## already called out in cacheSolve)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$get_inverse()
        if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$set_inverse(inverse)
        inverse
}

