## Put comments here that give an overall description of what your
## functions do
#
# These functions are used to create a special object that stores an 
# invertible  matrix and to calculate the matrix inverse, taking it from 
# the cache if it has already been calculated .
#


## Write a short comment describing this function
# This function creates a special object that stores a matrix 
# that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
      
      # initialize matrix
      inv <- NULL
      # define function to set the value of the matrix
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      
      # define function to get the value of the matrix
      get <- function() x
      
      # define function to set the value of the inverse
      setinverse <- function(inverse) inv <<- inverse
      
      # define function to get the value of the inverse      
      getinverse <- function() inv
      
      # define list of functions to manage the matrix and its inverse
      list(set = set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
# This function calculates the inverse of the matrix returned by 
# makeCacheMatrix function. If the inverse has already been calculated 
# this function should retrieve the inverse matrix from the cache.
#
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()

      # If the inverse matrix does already exist...
      if(!is.null(inv)){
            message("Retrieving cached data")
            # return inverse from the cache
            return(inv)
      }
      # Otherwise...
      # ... get matrix data ...
      mtxData <- x$get()
      # ...calculate inverse matrix...
      inv <- solve(mtxData,...)
      # save inverse matrix in cache
      x$setinverse(inv)
      #return inverse matrix
      inv
}
