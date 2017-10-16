# cachematrix.R
#
# Copyright (C) 2017 Andrew Tarver
# Author: Andrew Tarver (http://github.com/soitknows)
# Style Guide: https://google.github.io/styleguide/Rguide.xml
#
# R Programming - Johns Hopkins University
# Programming Assignment 2 - Lexical Scoping.
#
# Description:
#
# Example:
#
#   > test <- matrix( c(5, 1, 0, 3,-1, 2, 4, 0,-1), nrow=3, byrow=TRUE)
#   > m <- MakeCacheMatrix(test)
#   > CacheSolve(m)
#   > CacheSolve(m)
#          [,1]    [,2]   [,3]
#   [1,] 0.0625  0.0625  0.125
#   [2,] 0.6875 -0.3125 -0.625
#   [3,] 0.2500  0.2500 -0.500
#   > CacheSolve(m)
#   Getting cached data...
#          [,1]    [,2]   [,3]
#   [1,] 0.0625  0.0625  0.125
#   [2,] 0.6875 -0.3125 -0.625
#   [3,] 0.2500  0.2500 -0.500


MakeCacheMatrix <- function(mx = matrix()) {
  # Creates a special cached object for use in the CacheSolve function.
  #
  # Args:
  #   mx: a maxtrix of variable dimensions, but than can be inverted.
  #
  # Returns:
  #
  
  inverse <- NULL
  set <- function(setter) {
    mx <<- setter
    inverse <<- NULL
  }
  get <- function() mx
  set.inverse <- function(solve) inverse <<- solve
  get.inverse <- function() inverse
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}



CacheSolve <- function(cached, ...) {
  #
  #
  # Args:
  #   cached:
  #
  # Returns:
  #
  
  inverse <- cached$get.inverse()
  if(!is.null(inverse)) {
    message("Getting cached data...")
    return(inverse)
  }
  data <- cached$get()
  inverse <- solve(data, ...)
  cached$set.inverse(inverse)
  inverse
}




