## These functions allow for potentially expensive matrix
## inverse calculations to be cached for improved performance
## on subsequent calls

## Creates a 'Cache Matrix' object by wrapping a  matrix in a list
## containing functions used to get and set the matrix and it's inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    get <- function() x
    set <- function(y) {
        inv <<- NULL
        x <<- y
    }
    getInverse <- function() inv
    setInverse <- function(inverse) inv <<- inverse
    list(get = get, set = set,
         getInverse = getInverse,
         setInverse = setInverse)
}


## Solves the inverse of a Cache Matrix (see above).
## The result is stored in the Cache Matrix so that on subsequent
## calls with the same matrix, the cached result is returned instead
## of re-calculating the inverse
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
