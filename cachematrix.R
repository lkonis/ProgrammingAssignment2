## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function create an object of a special matrix that can cache its inverse
## It allows setting the matrix or it's inverse
makeCacheMatrix <- function(x = matrix()) {
  # initialize the inverse of matrix to null
  invM <- NULL
  # whenever matrix content is changed, reset the inverse value (that is, flush the cache)
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  # with this we get the current matrix
  get <- function() {
    x
  }
  # with this we call the special inverse function
  setInv <- function(inv) invM <<- inv
  
  # 
  getInv <- function() invM
  
  # this defines the interface for object of this class
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)

}


## Write a short comment describing this function
# This function gets a special matrix object of class makeCacheMatrix as input
#    it assumes the matrix is reversible
# The output y is the inverse matrix such that the product of input and output:
# y %*% x   will give the unit matrix of same dimension (with 'ones' on its diagonal)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## check validity of matrix (non empty)
  if (all(is.na(x$get())))
  {
    return(NULL)
  }
  ## check if already in cache
  if (is.null(x$getInv()))
  {
    # inverse the input
    return(solve(x$get()))
  }
  ## if in cache, return existing value
  return(x$getInv())
}
