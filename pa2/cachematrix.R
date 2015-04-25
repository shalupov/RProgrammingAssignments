## Cache the inverse value of a matrix to avoid calculating it each time
## which is an expensive operation.

## This function returns a list of functions (set, get, getinverse, setinverse)
## to work on a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Storage for inverse value
  inverse <- NULL
  
  # Getter and setter for matrix value
  # Note: setting matrix value invalidates inverse value cache by setting it to NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  
  # Getter and setter for inverse value
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  
  # Getters and setters packed together to work on "x" and "inverse" variables
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## returns the inverse from the cache store within special "matrix"
cacheSolve <- function(x, ...) {
  # Get stored inverse value, return it if it's not null
  # Not null inverse value indicates that inverse matrix was already calculated before
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  # First time calculating inverse value
  # Calls to solve and stores result in a cache
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  
  inverse
}
