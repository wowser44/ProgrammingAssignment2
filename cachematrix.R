## Programming Assignment 2 - Caching the inverse of a matrix
## Modified the makeVector code from the example in the assignment

## Function to create a matrix, calculate the inverse of that matrix and store it in the cache

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
      setinverse = setinverse,
      getinverse = getinverse)
  }

## Function to check if the inverse matrix is in cache. If so, return it; if not, calculate it
cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
