## The functions allow to use the cached value to get the inverse matrix faster
## if it has already been computed earlier

## Contains functions to set/get the value of the matrix, set/get the value of the inverse matrix

makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  set <- function(y) {
    m <<- y
    inv <<- NULL
  }
  get <- function() m
  setinv <- function(inp_inv) inv <<- inp_inv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Return a matrix that is the inverse of 'x'
## Uses the cached value if possible
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(is.null(inv)) {
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
  } else {
    message("getting cached data")
  }
  inv
}
