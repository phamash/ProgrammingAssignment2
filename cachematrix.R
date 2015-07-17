## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## These are 2 functions  that cache and comput the inverse of a matrix
## The function creates a 'matrix' object that caches its inverse

makeCacheMatrix <- function(mtrx = matrix()) {
  inverse <- NULL ## initializing
  set <- function(x) {
    mtrx <<- x;
    inverse <<- NULL;
  }
  ## Get and Set functions for the inverse
  get <- function() return(mtrx);
  setinv <- function(inv) inverse <<- inv;
  getinv <- function() return(inverse);
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))

}


## Write a short comment describing this function
## The function below computes the inverse of the 'matrix' that is returned by "makeCacheMatrix" from above
## If the inverse has already been calculated, and the matrix hasn't changed, then this function "cacheSolve" will retrieve the inverse from the cache

cacheSolve <- function(mtrx, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- mtrx$getinv()
  if(!is.null(inverse)) {         ## if the inverse is not null, prints the message below and returns inverse
    message("Getting cached data...")
    return(inverse)
  }
  
  data <- mtrx$get()
  inverse <- solve(data, ...)
  mtrx$setinv(inverse)
  return(inverse)
}
