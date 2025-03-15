## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a special "matrix" object that can cache its inverse.
  
  makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Store the cached inverse
    
    # Function to set the matrix
    set <- function(y) {
      x <<- y
      inv <<- NULL  # Reset inverse when matrix changes
    }
    
    # Function to get the matrix
    get <- function() x
    
    # Function to set the inverse
    setInverse <- function(inverse) inv <<- inverse
    
    # Function to get the inverse
    getInverse <- function() inv
    
    # Return a list containing the functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }
  
  ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
  ## If the inverse has already been calculated (and the matrix has not changed), 
  ## then cacheSolve should retrieve the inverse from the cache.
  
  cacheSolve <- function(x, ...) {
    inv <- x$getInverse()  # Check if inverse is already cached
    
    if (!is.null(inv)) {
      message("Getting cached data")
      return(inv)
    }
    
    # Get the matrix and compute its inverse
    data <- x$get()
    inv <- solve(data, ...)
    
    # Cache the inverse
    x$setInverse(inv)
    
    return(inv)
  }
  

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}

