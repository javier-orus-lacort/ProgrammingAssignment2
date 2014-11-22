## These are two functions used to compute the inverse of a 
## matrix and to cache that for later uses, without having
## to compute that all the times.


## It creates an "special" matrix to store 
## the initial matrix and its cached inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## Return a list of functions for matrix and cached inverse handling 
        
        inverse <- NULL                                 # Variable defined to store the cached inverse.
        
        set <- function(y) {                            # Set the initial matrix to the parameter value and its cached inverse to NULL.
                x <<- y                                 # Superassignment operator "<<-" is used to modify the variables in the parent environment.
                inverse <<- NULL
        }
        
        get <- function() x                             # Retrieve and show the matrix.
        
        setinverse <- function(inv) inverse <<- inv     # Set the cached inverse to the parameter value, using the "<<-" operator to change the variable in the parent environment.
        
        getinverse <- function() inverse                # Retrieve and show the cached inverse.
        
        list(set = set, get = get,                      # List the functions of this "special" matrix created and make them available for later uses.
                setinverse = setinverse,
                getinverse = getinverse)
}


## It returns the cached inverse if it exists, 
## otherwise it computes and returns the inverse,
## storing that as cached inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inverse <- x$getinverse()                       # Get the cached inverse.
        
        if(!is.null(inverse)) {                         # Check whether the cached inverse exists or not.
                message("getting cached data")
                return(inverse)                         # If the cached inverse exists, then it returns and shows that value.
        }
        
        data <- x$get()                                 # If the cached inverse does not exists, then it computes effectively the inverse and sets the cached matrix to that value.
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse                                         # Return and show the computed inverse matrix.
}