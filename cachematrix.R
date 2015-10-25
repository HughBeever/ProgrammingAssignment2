## The makeCacheMatix function takes a matrix as input and stores the data in its environment. It also creates "get"
## and "set" pairs for the matrix and its inverse.
##
## The cacheSolve function makes use of the methods created by makeCacheMatrix to store an enduring copy of the inverse
## of the original matrix and to return this instead of repeatedly calculating the inverse using solve().

## makeCacheMatrix. Input must be an invertible matrix.Stores matrix in function environment and sets inverse to NULL

makeCacheMatrix <- function(myMatrix = matrix()) {
  
  inverse <- NULL
  
  ## Make sure the supplied matrix is invertible. If not, exit with message
  if (det(myMatrix) == 0) {
    message("Supplied matrix is not invertible")
  }
  else {
    ## create the set method to store the matrix and initialise the inverse
    set <- function(y) {
      myMatrix <<- y
      inverse <<- NULL
    }
    ## get method - return the original matrix
    get <- function() myMatrix
    ## setinverse method. Store the supplied value in the function environment (cache)
    setinverse <- function(invMatrix) inverse <<- invMatrix
    ## getinverse method. Return the inverse from the cache
    getinverse <- function() inverse
    ## expose the methods
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }
}


## cacheSolve: Takes a List created by makeCacheMatrix() as its input and calculates the inverse of the embedded matrix

cacheSolve <- function(inList) {
  ## Retrieve the inverse matrix from the cache
  i <- inList$getinverse()
  ## Check to see if anything was returned (cache hit)
  if(!is.null(i)) {
    message("getting cached data")
  }
  else {
    ## Cache miss, so calculate the inverse using solve() and store the calculated value in the cache
    data <- inList$get()
    i <- solve(data)
    inList$setinverse(i)
  }
  ## return the inverse matrix
  return(i)
}