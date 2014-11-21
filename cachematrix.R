## These functions Will create a matrix, Solve the matrix,
## store the cached Solve and return/recalculate Solve depending on x

## makeCacheMatrix function creates a matrix of x

makeCacheMatrix <- function(x = matrix()) {
  # sets m to NULL, used for caching
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # returns value of orig vector
  get <- function() x
  setinverse <- function(solve) m <<- solve   
  getinverse <- function(solve) m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns the cached value of Solve if x hasn't changed
## else returns the new calculated value of Solve if x has changed

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  #next section works if not null - won't calculate
  if(!is.null(m)) {
    message("getting cached data")
    return(m)   # returns cached value m
  }
  # next section calculates it 1st time or if changes m = NULL
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
