## Caching the Inverse of a Matrix

## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  
  ## Set method
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## Get method
  get <- function() {
    m
  }
  
  ## Set inverse method
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Get inverse method
  getInverse <- function() {
    i
  }
  
  ## Return list
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special matrix returned
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  
  ## Return inverse if it is already set
  if( !is.null(m) ) {
    return(m)
  }
  
  ## Get the matrix
  data <- x$get()
  
  ## Calculate the inverse
  m <- solve(data) %*% data
  
  ## Set the inverse
  x$setInverse(m)
  
  ## Return matrix
  m
  
}
