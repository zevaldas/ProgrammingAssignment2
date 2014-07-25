## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # Initialize value
        m <- NULL

        # Function which set the value of the matrix
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        
        # Function which get the value of the matrix
        get <- function() x
        
        # Function which set the value of the matrix inverse
        setinversematrix <- function(solve) m<<- solve
        
        # Function which get the value of the matrix inverse
        getinversematrix <- function() m
        
        # List of all functions which are decleared above
        list(set = set, get = get,
             setinversematrix = setinversematrix,
             getinversematrix = getinversematrix)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the 
## cachesolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        # Get inverse matrix from cache
        m <- x$getinversematrix()
        # Check if we have this matrix in the cache, if so
        # return a matrix that is the inverse of 'x' from cache
        if(!is.null(m)) {
              return(m)
        }
        # If matrix not in the cache, then calculate 
        # a matrix that is the inverse of 'x' 
        data <- x$get()
        m <- solve(data, ...)
        # Set matrix to the cache
        x$setinversematrix(m)
        # Return a matrix that is the inverse of 'x'
        m
}
