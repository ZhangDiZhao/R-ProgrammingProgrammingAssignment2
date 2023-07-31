## This function computes the inverse of the special "matrix" created by makeCacheMatrix

# makeCacheMatrix is a function that returns a list of functions
# Its puspose is to store a martix and a cached value of the inverse of the 
# matrix. Contains the following functions:
# setMatrix      set the value of a matrix
# getMatrix      get the value of a matrix
# cacheInverse   get the cahced value (inverse of the matrix)
# getInverse     get the cahced value (inverse of the matrix)

makeCacheMatrix <- function(a = numeric()) {
  
  # initially nothing is cached so set it to NULL
  cache <- NULL
  
  # store a matrix
  setMatrix <- function(newValue) {
    a <<- newValue
    # since the matrix is assigned a new value, flush the cache
    cache <<- NULL
  }
  
  # returns the stored matrix
  getMatrix <- function() {
    a
  }
  
  # cache the given argument 
  cacheInverse <- function(solve) {
    cache <<- solve
  }
  
  # get the cached value
  getInverse <- function() {
    cache
  }
  
  # return a list. Each named element of the list is a function
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


# The following function calculates the inverse of a "special" matrix created with it
cacheSolve <- function(b, ...) {
  # get the cached value
  inv <- b$getInverse()
  # if a cached value exists return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  # otherwise get the matrix, calculate the inverse and store it in the cache
  data <- b$getMatrix()
  inv <- solve(data)
  b$cacheInverse(inverse)
  
  # return the inverse
  inv
}