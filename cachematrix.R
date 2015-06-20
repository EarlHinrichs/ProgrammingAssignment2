## Goal: Create an enhanced matix object "CacheMatrix" which caches its inverse.
## The object created is a list of functions
##      get(): returns the matrix as a matrix class object
##      invert(): returns the inverse of the matrix.


## makeCacheMatrix creates an instance of our special matrix from an ordinary matrix
## Example usage: (x is an invertible matrix)
##  cacheMatrix <- makeCacheMatrix(x)   ##Create an instance of the special matrix from x
##  copyOfX <- cacheMatrix$get()        ##Get a copy of the original matrix
##  inverse <- cacheMatrix$invert()     ##Get the inverse, first time it is computed.
##  inverse <- cacheMatrix$invert()     ##Get the inverse, second time it fetches from cache


makeCacheMatrix <- function(x = matrix()) {
        ##Keep track of the matrix and it's inverse.
        ##Use lazy evaluation for the inverse. Only compute if necessary
    thisMatrix <- x
    inverse <- NULL
        ## return a copy of the original matrix
    get <- function(y) thisMatrix
        ## return the inverse of the matrix.
        ## compute if necessary, otherwise use cache copy
    invert <- function() {
        if( is.null(inverse) ) {
            inverse <<- solve(thisMatrix)
        }
        inverse
    }
        ##return a list with the get and invert functions
    list(get=get, invert=invert)
}


## cachesolve returns the inverse of a cachematrix that was created
## with makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    x$invert()
}

