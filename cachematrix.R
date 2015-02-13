## Put comments here that give an overall description of what your
## functions do

## Generate a set of functions to get the inverse of the matrix, wich caching

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get
       , setInverse = setInverse, getInverse = getInverse)
}


## Test case to demonstrate the functionality of the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
          message('getting cached data')
          return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}

## Test code. For each of x and y, the cacheSolve() call will return 1st the inverse matrix,
## and the 2nd time it will print the "getting cached data" message
# x <- makeCacheMatrix(matrix(c(1,2,3,4), 2, 2))
# cacheSolve(x)
# cacheSolve(x)
# y <- makeCacheMatrix(matrix(c(5,6,7,8), 2, 2))
# cacheSolve(y)
# cacheSolve(y)
