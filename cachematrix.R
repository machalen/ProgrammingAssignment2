## For the Programming assignment 2 it was required to build two functions
##allowing to cache the value of the inverse of a matrix if it was previously 
##calculated. The value of the inverse is stored if it is calculated for the first time.

## The first function makeCacheMatrix creates a special matrix that. This function sets 
##the value of the matrix, gets the value of the matrix, sets the value of the mean and
## gets the value of the mean when in it was already calculated.

makeCacheMatrix <- function(x = matrix()) {
        inve <- NULL
        set <- function(y) {
                x <<- y
                inve <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inve <<- inverse
        getinv <- function() inve
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}



## The aim of the second function is to use the special matrix created before in order
##to obtain the inverse of the matrix. It first checks if the inverse has been already
##calculated, and if so it gets the value from the cache. Otherwise, calculates the inverse
##and stores that matrix in the cache.

cacheSolve <- function(x, ...) {
        inve <- x$getinv()
        if(!is.null(inve)) {
                message("getting cached data")
                return(inve)
        }
        data <- x$get()
        inve <- solve(data)
        x$setinv(inve)
        inve
}
