## Put comments here that give an overall description of what your
## functions do

#makeCacheMatrix: builds matrix objects with functions to cache inverse
#cacheSolve: computes inverse of matrix object returned by makeCacheMatrix

## Write a short comment describing this function
#  creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        #set values of matrix and initially the inverse to NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        #returns the matrix
        get <- function() x
        #sets value of Inverse to the value given to function
        setInverse <- function(inv) inverse <<- inv
        #returns the inverse of matrix
        getInverse <- function() inverse
        
        #returns list of functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Write a short comment describing this function
#computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #get Inverse of matrix
        inv <- x$getInverse()
        #if inverse has already been set -> use cached data and return inverse
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        #if inverse has not been set yet: get data and calculate inverse
        #to set inverse in list and return inverse
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
        
}
