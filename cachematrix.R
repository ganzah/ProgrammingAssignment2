## Put comments here that give an overall description of what your
## functions do

## Returns a list containing functions to set and get a matrix value and its inverse

makeCacheMatrix <- function(x = matrix()) {
  x_inverse <- NULL
  set <- function(n) {
    x <<- n
    x_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) x_inverse <<- inv
  getinverse <- function() x_inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Solves a cacheable matrix (that is, a matrix that was passed to the makeCacheMatrix function),
## calculating its inverse and storing it in a cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  x_inverse <- x$getinverse()
  if(!is.null(x_inverse)) {
    return(x_inverse)
  }
  data <- x$get()
  x_inverse <- solve(data, ...)
  x$setinverse(x_inverse)
  x_inverse
}
