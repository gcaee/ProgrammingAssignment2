## Put comments here that give an overall description of what your
## functions do

# Input your invertible matrix and it would calculate the inverse for you
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) x <<- y
    get <- function() x
    set_inv <- function(INV) inv <<- INV
    get_inv <- function() inv
    
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}

# Put your makeCacheMatrix as input to retrieve the inverse
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$get_inv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$set_inv(inv)
    inv
}