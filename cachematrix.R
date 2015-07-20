## R Programming Assignment 2
## Sarah Michel
## 2015-07-19

## makeCacheMatrix creates a special matrix object that is capable of
## caching its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL #Initialize the object with nothing as its inverse
    set <- function(y) { 
        x <<- y #Set x equal to whatever it passed to set
        i <<- NULL #Overwrite previously cached inverse with NULL
    }
    get <- function() x #Returns the matrix
    setinverse <- function(inverse) i <<- inverse #Caches inverse
    getinverse <- function() i #Returns the inverse
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

## cacheSolve computes the inverse of the special matrix object created
## by makeCacheMatrix. If the inverse has already been calculated (and
## the matrix has not changed), cacheSolve will simply retrieve the
## inverse from the cache. Otherwise it will calculate it and store it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse() #Retrieves inverse from cache
    if(!is.null(i)) { #If something is there, returns it and stops
        message("getting cached data")
        return(i)
     }
    data <- x$get() #If something is not stored in i, gets x
    i <- solve(data,...) #Computes the inverse of x
    x$setinverse(i) #Stores inverse of x into cache for next time
    i
}
