## cachematrix.R
## 23 Nov 2014; phopkins108
##
## Two functions are used to load a matrix, in order to
## compute, cache, and retrieve the inverse matrix

## creates a special matrix object, which can
## cache and retrieve a matrix and it's inverse
##
makeCacheMatrix <- function(x = matrix()) {
    inv_matrix <- NULL

    set <- function(y) {           # save data matrix to cache
        x <<- y
        inv_matrix <- NULL
    }

    get <- function() x             # get the data matrix

    # save inverse matrix to cache
    set_inv_matrix <- function(solve) inv_matrix <- solve

    # retrieve inverse matrix from cache
    get_inv_matrix <- function() inv_matrix

    list( set = set, get = get, set_inv_matrix = set_inv_matrix,
            get_inv_matrix = get_inv_matrix )

} # end makeCacheMatrix()

## 1) computes the inverse of a matrix;
## 2) retrieves a previously cached inverse matrix
##
cacheSolve <- function(x, ...) {
    inv_matrix <- x$get_inv_matrix ()

    if ( !is.null(inv_matrix) ) {
        message("retrieving cached inverse matrix")
    } else {
        matrix_data <- x$get()
        inv_matrix <- solve(matrix_data, ...)
        x$set_inv_matrix(inv_matrix)
    }

    ## Return a matrix that is the inverse of 'x'
    return(inv_matrix)

} # end cacheSolve()