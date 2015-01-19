## Put comments here that give an overall description of what your
## functions do

## This function creates a 'special matrix' object that can cache its inverse
## The function itself returns a list of set() and get() functions that allow setting  (and reading) the matrix or it's inverse
makeCacheMatrix <- function(x = matrix()) {
  # initialize the inverse of matrix to null
  invM <- NULL
  
  # set new value to the matrix
  # whenever the matrix content is changed, reset the inverse value (that is, flush the cache)
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  # read the current matrix's value
  get <- function() {
    x
  }
  # set new value to the inverse-matix field
  # typically we will call cacheSolve() to set this field
  setInv <- function(inv) invM <<- inv
  
  # reading the inverse-matrix
  getInv <- function() invM
  
  # this defines the interface for object of this class
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)

}


# This function gets a special matrix object of class makeCacheMatrix as input
#    it assumes the matrix is reversible
# The output is the inverse matrix such that the product of input and output:
# y %*% x   will give the unit matrix  (with 'ones' along its diagonal) of same dimension

cacheSolve <- function(x, ...) {
  # check validity of matrix (non empty)
  if (all(is.na(x$get())))
  {
    return(NULL)
  }
  # check if not already in cache
  if (is.null(x$getInv()))
  {
    # inverse the input and return
    return(solve(x$get()))
  }
  # if in cache, return existing value
  return(x$getInv())
}
