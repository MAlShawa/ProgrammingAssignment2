## The main objective of the following functions is to help cache the inverse 
## of a supplied matrix, instead of re-computing it repeatedly 
## assumptions:  - the supplied matrix, to be inverted, is assumed to be:
##                 1) a square matrix; and 2) invertible
##               - Computing the inverse is done by using the solve() function.      


## makeCacheMatrix: creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    set <- function(y) {
        x <<-    y
        inv_x <<-   NULL     # guarantees setting/resetting the inverse matrix
                             # whenever the matrix is set/changed
    }
    get <- function() x
    set_inv <- function(inv_mat) inv_x <<- inv_mat
    get_inv <- function() inv_x
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}


## cacheSolve: computes the inverse of the special "matrix" returned by 
##            makeCacheMatrix above, if it not been computed and cached already; 
##            then it should set/cache and return the retrieved/computed inverse 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_x <- x$get_inv()
    if(!is.null(inv_x)) {
        message("getting cached inv_mat")
        return(inv_x)
    }
    mat <- x$get()
    inv_x <- solve(mat, ...)
    x$set_inv(inv_x)
    inv_x
}