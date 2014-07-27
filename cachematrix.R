## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    invrsMtrx <- NULL
    set <- function(y) {
        x <<- y
        invrsMtrx <<- NULL
    }
    get <- function() x
    setinv <- function(invrs) invrsMtrx <<- invrs
    getinv <- function() invrsMtrx
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invrsMtrx <- x$getinv()
    if(is.matrix(invrsMtrx)) {
        message("Info: Getting cached data ...")
        return(invrsMtrx)
    }
    data <- x$get()
    invrsMtrx <- solve(data, ...)
    x$setinv(invrsMtrx)
    invrsMtrx
}
