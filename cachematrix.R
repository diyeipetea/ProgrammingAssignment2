
## Write a short comment describing this function
### I'm creating makeCacheMatrix... 
### This function creates a special "matrix" object that can cache its inverse.
### It contains four functions:
### set: Sets the value of the matrix.
### get: Gets the value of the matrix.
### setInverse: Sets the value of the inverse.
### getInverse: Gets the value of the inverse.
### It returns a list of these functions.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize inverse as NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset inverse when new matrix is set
    }
  get <- function() x  # Function to get matrix
  
  setInverse <- function(inverse) inv <<- inverse  # Function to set inverse
  
  getInverse <- function() inv  # Function to get cached inverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
### cacheSolve:
### This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
### It first checks if the inverse has already been calculated and cached.
### If the inverse is cached, it retrieves the inverse from the cache and returns it.
### If the inverse is not cached, it calculates the inverse using the solve function, caches the result, and then returns the inverse.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)  # Return cached inverse if available
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)  # Compute inverse
  x$setInverse(inv)  # Cache the inverse
  
  inv
}
# An example of using makeCacheMatrix and CaheSolve
# Create a special "matrix" object
matrix_obj <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2))
print(matrix_obj)
# Compute and cache the inverse
inverse_matrix <- cacheSolve(matrix_obj)
print(inverse_matrix)

# Retrieve the cached inverse
inverse_matrix_cached <- cacheSolve(matrix_obj)
print(inverse_matrix_cached)
print(inverse_matrix)
