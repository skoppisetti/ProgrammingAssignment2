## cachematrix.R implements a wrapper for the matrix object capable of storing its inverse and 
## a wrapper for the solve() method which first checks to see if the invese is already cached
## otherwise performs the matrix inversion.
## This is implemented using two functions makeCacheMatrix() and cacheSolve()


## makeCasheMatrix() is a wrapper to create a matrix object
## This provides plumbing required to cache the inverse of the matrix
## and exposes the get and set methods for the matrix and its inverse as a list

makeCacheMatrix <- function(m = matrix()) {
        minv <- NULL  ## initialize the inverse to NULL
        
        ## 'set'ter function for the matrix itself
        set <- function(matrix) {
                m <<- matrix
                minv <<- NULL
        }
        
        ## 'get'ter function to return the original matrix
        get <- function() m
        
        ## 'set'ter function for the inverse matrix
        setinv <- function(inv) minv <<- inv
        
        ## 'get'ter function for the inverse matrix
        getinv <- function() minv
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)        
}


## cacheSolve() is the actual wrapper to the Solve() function
## This function first checks if the matrix passed already has a calculated inverse
## If the matrix already has a inverse the cached inverse is returned 
## Otherwise the Solve is invoked to calcuate the inverse and cache it 

cacheSolve <- function(m, ...) {
        inv <- m$getinv()
        
        # Check to see if there is a previously cached inverse
        if(!is.null(inv)) {
                message("Cached inverse matrix found. Returning the cached inverse.")
                return(inv)
        }
        
        # If no cached inverse is found, calculate the inverse using solve() and cache it
        mat <- m$get()
        inv <- solve(mat, ...)
        m$setinv(inv)
        inv
}
