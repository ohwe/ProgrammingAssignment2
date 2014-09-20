##  creates an (empty) "matrix object" with list of "matrix methods"

makeCacheMatrix <- function(x = matrix()) {
    x <- NULL
    set <- function(y) {
        x <<- as.matrix(y)
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- as.matrix(inverse)
    getInv <- function() inv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
    
    
}

## retrieves already prepared inverse matrix or calculates it from scratch if there is none

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    ma <- x$get()
    inv <- solve(ma)
    x$setInv(inv)
    inv
}
    