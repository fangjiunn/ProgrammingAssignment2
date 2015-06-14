## this pair of functions will cache matrices' inverses
## once they have been found

## makes a Matrix that also caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<-NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Finds the inverse and updates cacheMatrix if needed
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) { return(i) }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
