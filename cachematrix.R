makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize a variable to store the inverse matrix
  
  # Function to set the matrix and reset the cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the cached inverse when a new matrix is set
  }
  
  # Function to get the matrix
  get <- function() x  
  
  # Function to set the inverse matrix in the cache
  setInverse <- function(inverse) inv <<- inverse  
  
  # Function to get the cached inverse matrix
  getInverse <- function() inv  
  
  # Return a list of functions for setting and getting matrix and its inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  # Check if the inverse is already cached
  if (!is.null(inv)) {
    message("Getting cached inverse matrix...")
    return(inv)  # Return the cached inverse if it exists
  }
  
  # Retrieve the original matrix
  mat <- x$get()
  
  # Compute the inverse matrix
  inv <- solve(mat, ...)  
  
  # Store the computed inverse in the cache
  x$setInverse(inv)  
  
  # Return the inverse matrix
  inv
}


