## These functions solve for the inverse of a matrix using caching.  
## If the inverse has not been computed, it will be computed.  If it has already
## been computed, the result will be retrieved from cache.

## This function creates a special "matrix", which is really just a list of 
## functions to 1) set the matrix, 2) get the matrix, 3) set the inverse, and 
## 4) get the inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {rnorm
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the user-defined matrix.  However, it
## first checks to see if the result is in cache; if so, it will skip the computation.
cacheSolve <- function(x, ...) {
        m <- x$getinv()
        
        ## The inverse was found in cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## get the used-defined matrix
        data <- x$get()
        
        ## Compute the inverse
        m <- solve(data, ...)
        
        ## Set the result in cache
        x$setinv(m)
        m
}