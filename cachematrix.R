## makeCacheMatrix and cacheSolve functions are used to cache the inverse of matrix. These functions will help to eliminate
## repeated computation when not necessary by getting the value from cache when available.

## makeCacheMatrix creates a special matrix that contains/returns list of functions
## setMatrix sets the value of matrix
## getMatrix gets the value of matrix
## setInverse sets the value of inverse of matrix
## getInverse gets the value of inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  setMatrix<-function (y){
    x<<-y
    inv<<-NULL
    
  }
  getMatrix<-function() x
  
  
  setInverse<-function(inverse){
    inv<<- inverse
  }
  
  getInverse<-function() inv
  
  
  list(setMatrix=setMatrix, getMatrix=getMatrix, setInverse=setInverse, getInverse=getInverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has already 
## been calculated (and the matrix has not changed), then the cacheSolve retrieves the inverse from cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getInverse()
  
    
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
    
  }
  m<-x$getMatrix()
  inv<-solve(m, ...)
  x$setInverse(inv)
  inv
}
