## This script calculates and caches the inverse of a invertible matrix
## Use syntax
## x <- matix(.....) # define the matrix
## inverse_x <- cacheSolve(makeCacheMatrix(x))

## This function gets and sets an ivertible matrix "mat" and it's inverse "inv_mat", outputting a list of these four operations

makeCacheMatrix <- function(x = matrix()){
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


## This functions takes in the function list creates in the above function and outputs the inverse of the matrix "mat"

cacheSolve <- function(x,...){
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
