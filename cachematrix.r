##  Cachematrix.R
##  M :Matrix
##  cacheMatrix <- makeCacheMatrix(M)
##  cacheSolve(cacheMatrix)
##  cacheMatrix$setInverse(solve(data, ...)) # Private function containing cached inverse of x
##  cacheMatrix$getInverse()                 # Private function used to get the cached inverse of x

## Create a cacheMatrix object for an invertale matrix.

makeCacheMatrix <- function(x = matrix()) {
        cachedInverse <- NULL
        set <- function(y) {
                x <<- y
                cachedInverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) cachedInverse <<- inverse
        getInverse <- function() cachedInverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Return the inverse of an cacheMatrix object

cacheSolve <- function(x, ...) {
        inverseFunc <- x$getInverse()
        if(!is.null(inverseFunc)) {
                message("getting cached data")
                return(inverseFunc)
        }
        data <- x$get()
        inverseFunc <- solve(data, ...)
        x$setInverse(inverseFunc)
        inverseFunc
}
        data <- x$get()
        invFunc <- solve(data, ...)
        x$setInverse(invFunc)
        invFunc
}
