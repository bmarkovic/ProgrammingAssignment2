## Implents an object (list) that expands matrices
## with matrix inverse caching functionality

## Takes a matrix as an argument. Returns a list functioning as
## an object extending matrix functionality with getters, setters
## and ability to store/retreive cached copy of matrice's inverse
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}

## Returns a matrix that is the inverse of argument. Takes a makeCacheMatrix
## object as an argument. If the argument contains a matrix in x$inv, it is
## returned without performing solve()
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  
}
