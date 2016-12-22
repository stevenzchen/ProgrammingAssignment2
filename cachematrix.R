## The following functions cache the inverse of a matrix
## within a list of get and set functions.

## makeCacheMatrix creates a matrix list object with get and set methods
## for the matrix and its cached inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <- NULL
    }
    get <- function() {
        x
    }
    setinverse <- function(inverse) {
        inv <<- inverse
    }
    getinverse <- function() {
        inv
    }
    list(set = set, get = get, setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve takes a special matrix list object and gets the inverse
## by retrieving from cache or solving and storing in cache

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    inv
}
