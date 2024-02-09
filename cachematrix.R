##makeCacheMatrix creates a special "matrix" object with functions to cache and compute its inverse.
##cacheSolve computes and caches the inverse of the special "matrix" object.


makeCacheMatrix <- function(x = matrix()) {
  # creating a function called makeCacheMatrix
  
  
  invX <- NULL

  # invX: the inverse of the matrix, or NULL if not yet calculated

  # set function updates the value of x in the cache and sets invX to NULL
  set <- function(y) {
    x <<- y
    invX <<- NULL
  }

  # get function retrieves the value of x from the cache
  get <- function() x

  # inv function computes the inverse of the matrix in the cache
  
  inv <- function() {
    if (!is.null(invX)) {
      # Inverse has already been calculated, return it from the cache
      return(invX)
    }
    invX <- solve(x)
    return(invX)
  }

  # Return the cache object
  list(set = set, get = get, inv = inv)
}

cacheSolve <- function(cache, ...) {
  # Creating a function called cacheSolve
  if (is.null(cache$inv())) {
    # Inverse has not been calculated yet, so calculate it and store it in the cache
    cache$inv <- solve(cache$get(), ...)
  }
  # Return the inverse from the cache
  cache$inv()
}