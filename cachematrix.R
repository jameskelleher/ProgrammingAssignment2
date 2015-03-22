## makeCacheMatrix returns a list a funcitons that can reference a matrix and its inverse
## cacheSolve solves matrix by accessing cached inverse (or calculating inverse if no cache)

## inv stands for inverse, is initialized to NULL
## get() returns the matrix
## set() sets the matrix
## getinv() returns the inverse
## setinv() sets the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverted_matrix) inv <<- inverted_matrix
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## x is the list that refers back to a cache matrix
## if the inverse of x is unknown, we calculate it using solve()
## otherwise, we simply return the inverse

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if (!is.null(inv)) {
          message('getting cached data')
          return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
