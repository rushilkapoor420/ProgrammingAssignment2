## The following functions provide a means to avoid calculating the inverse of any invertible matrix if it has already been calculated once before
##(by storing it in cache)


## makeCacheMatrix()
## creates a special object that stores an invertible matrix and also caches its inverse
## It basically returns the following list of functions 
##    1. setMatrix() : To store a matrix
##    2. getMatrix() : To retrieve the stored matrix 
##    3. setInverse() : To cache (store) the inverse of the matrix
##    4. getInverse() : To retrieve the cached inverse of the matrix
## Please note that it is assumed that the object passed to the setMatrix function is always an invertible matrix
makeCacheMatrix <- function(x = matrix()) {
 
  inv <- NULL
 
  setMatrix <- function(y) {
    
    if(length(dim(y))!=2){
      print("Assumption Violated: The matrix to be stored is not a square matrix!")
    }
    x <<- y
    ## Reset Cached inverse only if argument matrix is different than stored matrix  
    if(all(x!=y)){
      inv <<- NULL
    }
    
  }
  
  getMatrix <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv
  # returns a list of functions to set or retrieve the matrix or its cached inverse
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve()
## Returns the inverse of the argument matrix (x). It does this under one of the following conditions:
##    1. Retrieves the cached inverse of the matrix, if it exists
##    2. calculates the inverse of the matrix using solve() function and caches it, if it is not already cached
cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  invertibleMatrix <- x$getMatrix()
  inverse <- solve(invertibleMatrix)
  x$setInverse(inverse)
  inverse
}
