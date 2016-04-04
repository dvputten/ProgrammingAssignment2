## With the functions makeCacheMatrix and cacheSolve you can calculate and store
## the inverse of a matrix for future use.

## makeCacheMatrix stores a matrix called matrix_x and its inverse
## use the functions set and get for storing and retrieving the matrix
## use the functions set_inverse and get_inverse for storing and retrieving the inverse of the matrix

makeCacheMatrix <- function(matrix_x = matrix()) {
    inverse <- NULL
    set <- function(new_matrix) {
        matrix_x <<- new_matrix
        inverse <- NULL
    }
    
    get <- function() matrix_x
    set_inverse <- function(new_inverse) inverse <<- new_inverse
    get_inverse <- function() inverse
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)

}


## cacheSolve calculates and stores the inverse of a matrix stored in a
## makaCacheMatrix object. When the inverse is already calculated and stored,
## this function retrieves the stored inverse.

cacheSolve <- function(matrix_x, ...) {
        ## Return a matrix that is the inverse of 'matrix_x'
    inverse <- matrix_x$get_inverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- matrix_x$get()
    inverse <- solve(data)
    matrix_x$set_inverse(inverse)
    inverse
}
