## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## A pair of functions that cache the inverse of a matrix

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    ## Initialize the inverse matrix
    matrix_inversed <- NULL
    
    ## Method to set the matrix
    set <- function(m) {
        x <<- m
        matrix_inversed <<- NULL
    }
    ## Method the get the matrix
    get <- function(() x
    
    ## Method to set the inverse of the matrix                
    set_inverse <- function(inverse) matrix_inversed <<- inverse
    
    ## Method to get the inverse of the matrix    
    get_inverse <- function() matrix_inversed
    
    ## Return a list of the methods
    list(set = set, get = get,
         set_inverse = set_inverse,
         set_inverse = set_inverse)
}


## Write a short comment describing this function
## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cache_inverse <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'    
    matrix_inversed <- x$get_inverse()
    
    ## Return the inverse if its already set    
    if(!is.null(matrix_inversed)) {
        message("getting cached data")
        return(matrix_inversed)
    }
    
    ## Get the matrix from our object
    data <- x$get()
    
    ## Calculate the inverse using solve function (no other parameters than data provokes inversion)
    matrix_inversed <- solve(data, ...)
    
    ## Set the inverse to the matrix
    x$set_inverse(matrix_inversed)
    
    ## Return the matrix
    matrix_inversed
}