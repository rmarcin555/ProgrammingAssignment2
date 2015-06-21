## This module provides functions to create a new 
## better cached matrix object wich stores its inveresed 
## form and methods to create it. All is a form of list

## This function creates improved matrix object
## Args:
##  x: standard matrix object, if none empty one 
##      will be created
##
## Returns:
##  list represenging cached matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function calculates inverse matrix of given
## cached matrix. If cached data is already present
## it is being retrived and calculation is being ommitted
## Args:
##  x: cached matrix object (in R list form)
##  ... additional arguments which are going to be passed to solve() function
##
## Returns:
##  matrix (standard R matrix) object containing inversed matrix of 'x'

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
