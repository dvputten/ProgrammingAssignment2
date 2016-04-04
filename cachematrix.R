## The functions makeCacheMatrix and cacheSolve make it possible to calculate and store
## the inverse of a matrix


makeCacheMatrix <- function(x = matrix()) {
    # Stores a matrix together with its inverse when calculated
    # 
    # Args:
    #    x: an invertible matrix
    #
    # Returns:
    #    a list with getters and setters for the matrix and the inverse
    
    #initialise the inverse
    inverse <- NULL
    
    #setter function for the matrix and reset the inverse to NULL
    set <- function(new_matrix) {
        x <<- new_matrix
        inverse <<- NULL
    }
    
    #getter function for the matrix
    get <- function() x
    
    #setter function for the inverse
    set_inverse <- function(new_inverse) inverse <<- new_inverse
    
    #getter function for the inverse
    get_inverse <- function() inverse
    
    #return a list with the functions
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)

}


cacheSolve <- function(x, ...) {
    # Calculates and stores the inverse of a matrix in a makeCacheMatrix object
    #
    #Args:
    #    x: a makeCacheMatrix object containing an invertable matrix
    #
    # Returns
    #   the inverse of the matrix in x

    # get the inverse of x
    inverse <- x$get_inverse()
    
    # if the inverse was already stored, return it
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    # if the inverse was not stored: calculate and store it
    data <- x$get()
    inverse <- solve(data)
    x$set_inverse(inverse)
    
    #return the inverse
    inverse
}
