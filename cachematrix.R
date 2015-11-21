## Implents an object (list) that expands matrices
## with matrix inverse caching functionality

## Takes a matrix as an argument. Returns a list functioning as
## an object extending matrix functionality with getters, setters
## and ability to store/retreive cached copy of matrice's inverse
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL ## for caching inverse
  old <- x    ## caches matrix for comparison
  set <- function (y) {
    old <<- x
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  setold <- function(inverse) old <<- old
  getold <- function() old
  ## safer matrix comparison by Rui Barradas retrieved from:
  ## https://stat.ethz.ch/pipermail/r-help/2012-June/315408.html
  unchanged <- function() dim(x) == dim(old) && all(x == old)
  list(set = set, get = get, 
       setinverse = setinverse, 
       setold = setold, getold = getold, 
       getinverse = getinverse, 
       unchanged = unchanged)

}

## Returns a matrix that is the inverse of argument. Takes a makeCacheMatrix
## object as an argument. If the argument contains a matrix in x$inv, it is
## returned without performing solve()
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()

  if(!is.null(inv)) {
    if(x$unchanged()) { 
      message("getting cached data")
      return(inv)
    }
  }
  data <- x$get()
  x$setold(data)
  inv <- solve(data, ...)
  x$setinverse(inv)
}
