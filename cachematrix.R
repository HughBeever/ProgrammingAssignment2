## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(myMatrix = matrix()) {

    inverse <- NULL
    if (det(myMatrix) == 0) {
      message("Supplied matrix is not invertible")
    }
    else {
      set <- function(y) {
        myMatrix <<- y
        inverse <<- NULL
      }
      get <- function() myMatrix
      setinverse <- function(invMatrix) inverse <<- invMatrix
      getinverse <- function() inverse
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
    }
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'inMatrix'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  return(i)
  }
