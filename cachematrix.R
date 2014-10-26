## cachematrix.R - defines two functions as described below.
## makeCacheMatrix 
##    - this function defines a kind of matrix object that saves its inverse calculation.
##    - it defines two internal functions to deal with the actions of setting/getting the matrix and the inverse of it.             
## cacheSolve
##    - this function returns the inverse of a given matrix. 
##    - the matrix passed as parameter should be one created with makeCacheMatrix function. 

## makeCacheMatrix defines an object with an internal matrix and its inverse calculation
# The purpose is to avoid repeated calculations for the inverse of the matrix and returning the cached calculation whenever possible.
# It includes the functions: get, set and getInverse
makeCacheMatrix <- function(x = matrix()) {
  # initialize the inverse as NULL
  inverse <- NULL
  # initialize the internal matrix with the matrix passed as parameter 
  m <- x
  
  # getInverse function returns the cached inverse if it exists. 
  # If it doesnt exist, calculates it, caches and returns
  # Note: It assumes all matrixes being used are invertible. 
  getInverse <- function() {
    if (is.null(inverse)){ 
      # Inverse is not calculated. 
      # Calls the function that calculates it and caches for future calls.
      inverse <<- setInverse(m)
    }
    # returns the (now) cached data...
    inverse
  }
  
  # setInverse function, calculates the inverse of the matrix passed as parameter and caches it.
  setInverse <- function(p_matrix = matrix()) inverse <<- solve(p_matrix)

  # get function, returns the matrix from this object
  get <- function() as.matrix(m)
  
  # set function, sets the matrix for this object and invalidates the inverse cache if it exists.
  set <- function(p_matrix = matrix()) {
    m <<- p_matrix
    inverse <<- NULL
  }

  # list of public accessible functions.
  list(getInverse = getInverse, get = get, set = set)
}

## cacheSolve funtion returns the inverse of a matrix from the CacheMatrix object defined above.
cacheSolve <- function(x, ...) {
  ## Calls the getInverse function from the x paramater
  x$getInverse()
}
