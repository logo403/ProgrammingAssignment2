## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a 
## matrix rather than computing it repeatedly 

## This file contains a pair of functions that cache the 
## inverse of a matrix. 

## The first function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        # function to set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # function to get the matrix
        get <- function() x
        
        # function to set the inversion of the matrix
        setsolve <- function(solve) m <<- solve
        
        # function to get the inverted matrix
        getsolve <- function() m
        
        # put all functions into one list
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
}


## The second function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix(). 

## If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve()
## retrieves the inverse from the cache.

## See example on how to use this function below 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        
        # Verification if the matrix has already been inverted
        # if so get the inverted matrix from the cache and skip inversion
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # Complete the matrix inversion with the functions
        # listed in makeCacheMatrix()
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        
}

## Exemple on how to use cacheSolve() along with makeCacheMatrix()
## 1.Create a square matrix like
## > x<-matrix(rnorm(100*100,3,6),100,100)
## 
## 2. Assign the list from makeCacheMatrix
## > ma <- makeCacheMatrix(x)
##
## 3. Run the function cacheSolve on "ma" and assign result
## > invx<-cacheSolve(ma)
##
## Test 1: run the last line again to see if the message
## "getting cached data" will show up
## > invx<-cacheSolve(ma)
##
## Test 2; Verify if the product between invx and x gives the identity matrix
## > invx %*% x
