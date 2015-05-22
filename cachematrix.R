# Matrix inversion is usually a costly computation and there may be some benefit to 
# caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix 
# inversion that we will not discuss here). Your assignment is to write a pair of functions that 
# cache the inverse of a matrix.

# The function makeCacheMatrix creates a special "matrix" object that can cache its inverse
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix 
# 4. get the value of inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# The function cacheSolve first checks if the inverse of the matrix already been computed. If so, then it gets the results and skip the computation.
# If not, it computes the inverse and sets the inverse value in the cache using setInverse function.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    print("retrieve cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv  
}
