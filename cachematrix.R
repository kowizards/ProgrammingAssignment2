makeCacheMatrix <- function(x = matrix()) {
  # Create a matrix representation (represented by a list of functions) with the ability of caching its inverse.
  #
  # Args:
  #   x: The initial value of the created matrix,
  #      could be set by passing a normal R matrix value
  #      or left blank and set by the set function in the result List when use.
  #
  # Returns:
  #   The list of 4 interfaces for the created matrix:
  #       set: set the value of the matrix.
  #       get: get the value of the matrix.
  #       setInverse: set the inverse of the matrix, used by cacheSolve internally, users are not recommended to use this directly.
  #       getInverse: get the inverse of the matrix,
  #                   NULL if not been calculated before, user should use cacheSolve to calculate the inverse.
  #
  #   The documentation of the returned value could also be retrieved by the comment function. Use cat function to print the doc.
  
  inverse <- NULL # the cache of the matrix inverse
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function()
    x
  
  setInverse <- function(inv)
    inverse <<- inv
  
  getInverse <- function()
    inverse
  
  val <- list(set = set,
              get = get,
              setInverse = setInverse,
              getInverse = getInverse)
  
  comment(val) <- 
"This is a list of 4 interfaces for an internal matrix with a cached inverse:
  set: set the value of the matrix.
  get: get the value of the matrix.
  setInverse: set the inverse of the matrix, used by cacheSolve internally, users are not recommended to use this directly.
  getInverse: get the inverse of the matrix,
              NULL if not been calculated before, user should use cacheSolve to calculate the inverse."
  val
}

cacheSolve <- function(x, ...) {
  # Calculate the inverse of a matrix created by makeCacheMatrix, taking the advantage of the matrix cache.
  #
  # Args:
  #   x:    a matrix created by makeCacheMatrix.
  #   ...:  further arguments passed to or from other methods.
  #
  # Returns:
  #   The inverse of the input matrix
  
  inv <- x$getInverse()
  # return the cached inverse value if it exists
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # calculate the inverse, store the result into the matrix cache then return
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}


###test
library(assertthat)

unit_test <- function() {
  X <- makeCacheMatrix()
  X$set(matrix(c(5, 0, 0, 2), nrow=2))
  IX <- cacheSolve(X)
  assert_that(identical(IX, matrix(c(0.2, 0, 0, 0.5), nrow=2)))
  IX2 <- cacheSolve(X)
  assert_that(identical(IX2, matrix(c(0.2, 0, 0, 0.5), nrow=2)))
  
  assert_that(identical(X$get(), matrix(c(5, 0, 0, 2), nrow=2)))

  cat(comment(X))
  
  Y <- makeCacheMatrix(matrix(c(1, 1, 0, 2), nrow=2))
  IY <- cacheSolve(Y)
  assert_that(identical(IY, matrix(c(1, -0.5, 0, 0.5), nrow=2)))
  IY2 <- cacheSolve(Y)
  assert_that(identical(IY2, matrix(c(1, -0.5, 0, 0.5), nrow=2)))
  
  assert_that(identical(Y$get(), matrix(c(1, 1, 0, 2), nrow=2)))
}
###end test
