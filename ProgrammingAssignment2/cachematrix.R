## This function caches the

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv_mat <- NULL
    set <- function(y) {
        x <<- y
        inv_mat <<- NULL
    }
    get <- function() x
    setinv <- function(inverse){
        inv_mat <<- inverse
    }
    getinv <- function() inv_mat
    list(set = set, get = get,
    setinv = setinv,
    getinv = getinv)


}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv_mat <- x$getinv()
    if(!is.null(inv_mat)) {
        message("getting cached data")
        return(inv_mat)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
