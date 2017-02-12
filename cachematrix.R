setwd("~/DataScienceCourses/Data Science - Coursera/Course 2 - R Programming/Week3/ProgrammingAssignment2")

## OVERALL DESCRIPTION OF THE FUNCTIONS 
## Function 1 (makeCacheMatrix) sets a matrix object (x) enabling it to cache 
## its inverse, it does not compute anything.
## Function 2 (cacheSolve)  checks if inverse of the matrix has already been computed, 
## if yes, will return result from the "cache"
## if not, will compute the inverse & store it in the cache in order to be reused

## FUNCTION 1 - makeCacheMatrix, with the following objects 
# x is a square matrix that can be inverted

## We set then 4 functions that will be used as arguments in the cacheSolve function
# set (sets a new matrix)
# get (gets the new matrix)
# set_inv (sets the inverse of the matrix)
# get_inv (gest the inverse)

# variable 'inv' stores the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        ## superassignment (<<-) will set x to value y in other environments than its own
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set_inv <- function(inverse) inv <<- inverse
    get_inv <- function() inv
    list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)
}


## FUNCTION 2 - cacheSolve - 
## Computes the inverse of the x matrix created in function 1. If result already
## cached (previously computed, cached, and matrix is identical), it is displayed. 

cacheSolve <- function(x, ...) {
    ## Ellipses refer to attributes from makeCacheMatrix (set, ... get_inv)
    inv <- x$get_inv() #stores previously computed matrix in variable "inv"
    if (!is.null(inv)) { #does "inv" has a value? i.e. result already cached?
        message("getting cached data")
        return(inv) #if yes, return cached result, display statement
    }
    temp_mat <- x$get() #if not, calculate the inverse of the matrix and return result
    inv <- solve(temp_mat, ...)
    x$set_inv(inv)
    inv
}
