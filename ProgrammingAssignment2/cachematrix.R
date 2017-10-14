## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(m = matrix()) {
  inverse <- NULL
  set <- function(setter) {
    m <<- setter
    inverse <<- NULL
  }
  get <- function() m
  setInv <- function(solve) inverse <<- solve
  getInv <- function() inverse
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inverse <- x$getInv()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInv(inverse)
  inverse
}




