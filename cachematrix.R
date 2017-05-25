## R Programming Week 3, Assignment 2 - Lexical Scoping
# Calculate the inverse of a matrix, but only if you have to!

# This function allows caller to get/set the value and inverse value of a square matrix
makeCacheMatrix <- function(x = matrix()) {
  # Ensure input is valid matrix
  if (!is.matrix(x) || nrow(x) != ncol(x)) {
    stop("Input must be square matrix.")
  }
  # init cache variable for matrix inverse
  matrixinverse <- NULL
  
  # set/get functions for matrix
  setmatrix <- function(y) {
    x <<- y
    matrixinverse <<- NULL
  }
  getmatrix <- function()
    x
  
  # set/get functions for matrix inverse
  getinverse <- function()
    matrixinverse
  setinverse <- function(x)
    matrixinverse <<- x
  
  # define list of functions
  list(
    getmatrix = getmatrix,
    setmatrix = setmatrix,
    getinverse = getinverse,
    setinverse = setinverse
  )
}


# This function calculates the inverse of a matrix.
# If the inverse has already been calculated and cached,
# it will return this value instead of recalculating.

cacheSolve <- function(x, ...) {
  # If the inverse has already been calculated, we will retrieve and store it here
  matrixinverse <- x$getinverse()
  
  # Check to see if the inverse was cached
  if (!is.null(matrixinverse)) {
    # If so, return it
    message("returning cached data")
    return(matrixinverse)
    # If not, calculate it
  } else {
    matrixinverse <- solve(x$getmatrix())
    # Cache it
    x$setinverse(matrixinverse)
    # And return it
    return(matrixinverse)
  }
}
