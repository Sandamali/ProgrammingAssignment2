## 

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  #cache variable
  cache <- NULL
  
 #set the matrix
  setMatrix <- function(newValue) {
    x <<- newValue
    # matrix is assigned a new value, flush the cache
    cache <<- NULL
  }
  
  # returns the stored matrix
  getMatrix <- function() {
    x
  }
  
  # cache the given argument 
  cacheInverse <- function(solve) {
    cache <<- solve
  }
  
  # get the cached value
  getInverse <- function() {
    cache
  }
  
  # return a list
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix function. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve will retrieve the inverse from the cache

# Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
  inverse <- x$getInverse()
  # if a cached value exists return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  # else get the matrix, caclulate the inverse and store it in
  # the cache
  data <- x$getMatrix()
  inverse <- solve(data)
  x$cacheInverse(inverse)
  
  # return the inverse
  inverse
        
}

