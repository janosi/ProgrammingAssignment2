## cachematrix implements matrix inversion
## The module can compute the inverse of the input matrix, then it stores the input
## matrix and its inverse in a local cache
## Once the inverse is calculated for the matrix, it is stored. Whenever the
## inverse is asked again later, the cached value is returned

## The object created with makeCacheMatrix (e.g. a <-makeCacheMatrix (m), where m
## is a matrix) stores the function's argument (m) in stored_matrix in the
## environment of the created object.
## The result object itself is a list of functions that can work with stored_original
## and those functions manage the inverse of stored_matrix in the environment
## of the created object
## The stored_matrix can be changed with the function set_stored_matrix. In 
## this case the cached inverse (stored_inverse) is cleared, so the client shall
## calculate it again

makeCacheMatrix <- function(stored_matrix = matrix()) {
    stored_inverse <- NULL
    set_stored_matrix <- function(new_matrix) {
        stored_matrix <<- new_matrix
        stored_inverse <<- NULL        
    }
    
    get_stored_matrix <- function() stored_matrix
    
    set_inverse <- function(new_inverse) stored_inverse <<- new_inverse
    
    get_inverse <- function() stored_inverse
    
    list(set_stored_matrix = set_stored_matrix,
         get_stored_matrix = get_stored_matrix,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## cacheSolve takes an object created with makeCacheMatrix as its argument.
## If the makeCacheMatrix object has valid inverse matrix stored already
## cacheSolve returns with that cached inverse matrix
## Otherwise cacheSolve fetches the matrix stored in makeCacheMatrix object's
## environment, calculates the inverse of that, cahces the inverse in 
## makeCacheMatrix object, and returns with the calculated inverse

cacheSolve <- function(x, ...) {
    inverse <- x$get_inverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    stored_matrix <- x$get_stored_matrix()
    inverse <- solve(stored_matrix)
    x$set_inverse(inverse)
    inverse
}
