## The following functions provide a way to cache the result of the often costly process of 
## calculating the inverse of a matrix.

## makeCacheMatrix takes a matrix and returns a list of functions that both get and set the values of the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solved) inv <<- solved
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve takes a list from the makeCacheMatrix function and either returns the inverse from the cache or does the calculation itself.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
