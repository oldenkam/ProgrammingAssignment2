## makeCacheMatrics creates a special matrix that can cache it's own inverse
## this prevents potentially computationally intensive reruns of the 'solve'
## function by just storing the result in an attribute of the matrix.

## cacheSolve generates the values for the special matrix built above and 
## stores the values.

## Example matrix:
## A <- matrix( c(5, 1, 0,
##               3,-1, 2,
##               4, 0,-1), nrow=3, byrow=TRUE)

## This function creates a special matrix that can store values or 'cache'
## them for future use.
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


## This function checks if the inverse of the matrix has already been
## computed then, if not, runs the inverse matrix calculation and 
## caches the return for future use. 
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


