## Cache the Inversion of a Matrix
## Caching is usefull to minimize avoidable computation by storing 
## information in memory when no changes are verified on the input data.


## Overall Goal: Storing a Matrix and caching the reverse 

  ## Step 1: Create a Matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # Set the value of the matrix
  set <- function(y) {
  x <<- y
  m <<- NULL
  }
  # Get the value of the matrix 
  get <- function() x
  # Set the inverse of the matrix
  setInverse <- function(inverse) m <<- inverse
  # Get the inverse of the matrix
  getInverse <- function() m
  
  # Provide list with following functions
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



  ## Step 2: Return a matrix that is the inverse of 'x'
## 1) computing the inverse of the matrix created with 'makeCacheMatrix'.
## 2) option to retrieve from the cache if the inverse is already calculated.

cacheSolve <- function(x, ...) {
    # getting inverse of matrix 'x' -> cache contains information
    m <- x$getInverse() 
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    # getting inverse of matrix 'x' -> cache is empty 
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m  
}   
