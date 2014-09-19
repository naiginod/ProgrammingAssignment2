## The functions work together to do a few things including storing a matrix,
## taking an inverse of the stored matrix, then once called on caching and outputting the inverse

## This function contains the default NULL if the cache is empty and then takes the initial matrix
## given to it and creating the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## This function caches the inverse of the matrix as it has been assigned to a variable, then if called on
## more than once without changing the inital variable will regurgitate the inversed matrixtouc

cacheSolve <- function(x, ...) {
        m <- x$getInverse() 
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
