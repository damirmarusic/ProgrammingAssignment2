## The following two functions cache the inverse of a matrix
## in order to speed up processing.

## This function creates a "matrix" object (really a list with setters and
## getters for the matrix itself and its cached inverse).

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() {
        x
    }
    setinverse <- function(solve) {
        i <<- solve
    }
    getinverse <- function() {
        i
    }
    list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function leverages makeCacheMatrix to efficiently solve
## the inverses of matrices.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
