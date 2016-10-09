## this file contains functions for caching the result of calculating the inverse matrix of a given matrix

## returns a list of functions for setting, getting the value passed to this function and
## functions for getting/setting the inverse-matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    # setter function for x
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # getter for x
    get <- function() x
    # setter for the inverse-matrix of x
    setinverse <- function(inverse) inv <<- inverse
    # getter for  the inverse-matrix of x
    getinverse <- function() inv
    # return the list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve either returns the inverse-matrix of x directly 
## (if it was already calculated) or it calculates the inverse and returns it afterwards

cacheSolve <- function(x, ...) {
        # try to get the cached result
        inv <- x$getinverse();
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        # calculate the identity matrix with the correct size
        ident <- diag(nrow = nrow(data), ncol = ncol(data))
        # now the solve the linear equationsystem to calculate the inverse matrix
        inv <- solve(data, ident)
        # cache the result
        x$setinverse(inv)
        inv
}
