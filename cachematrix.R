## This script calculates and caches the inverse of a invertible matrix
## Use syntax
## x <- matix(.....) # define the matrix
## inverse_x <- cacheSolve(makeCacheMatrix(x))

## This function gets and sets an ivertible matrix "mat" and it's inverse "inv_mat", outputting a list of these four operations

makeCacheMatrix <- function(mat = matrix()){
        inv_mat <- NULL
        set <- function(y) {
                mat <<- y
                inv_mat <<- NULL
        }
        get <- function() mat
        setinv <- function(inverse){
                inv_mat <<- inverse
        } 
        getinv <- function() inv_mat
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This functions takes in the function list creates in the above function and outputs the inverse of the matrix "mat"

cacheSolve <- function(mat_vec,...){
        inv_mat <- mat_vec$getinv()
        if(!is.null(inv_mat)) {
                message("getting cached data")
                return(inv_mat)
        }
        data <- mat_vec$get()
        inv <- solve(data, ...)
        mat_vec$setinv(inv)
        inv
}
