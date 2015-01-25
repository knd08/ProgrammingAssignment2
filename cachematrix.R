## Calculating the inverse of a matrix can be an expensive operation. The 
## functions written below show a way of caching the inverse of a matrix so
## that it may be used repeatedly without having to compute it again. 



## makeCacheMatrix() takes an invertible matrix as input and returns a list
## of functions which do the following:
## set() - sets the value of the matrix
## get() - returns the value of the matrix
## setInverse() - sets the value of the inverse of the matrix
## getInverse() - returns the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse) 

}


## cacheSolve() takes the list returned by the above function as input and 
## returns the inverse of the matrix from the cache.
## If the inverse is not found, it calculates the inverse, caches it via the 
## setInverse() function and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
