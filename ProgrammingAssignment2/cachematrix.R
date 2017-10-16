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
#   The functions below allow a user to specify a numeric matrix, and 
#   then calculate the inverse of that matrix. Since inverting a matrix 
#   can be a resource intensive process, repeat calculations are avoided
#   by leveraging lexical scoping to cache the matrix inverse in a MakeCacheMatrix 
#   object once once it is calculated.
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
  # Takes a numeric matrix and returns an object that can be used to calculate 
  # the inverse of that matrix. Once calcualted by the CacheSolve function, the 
  # inverse matrix will be cached in the mx object of the MakeCacheMatrix function environment.
  #
  # Args:
  #   mx: a numeric maxtrix of variable dimensions, but than can be inverted.
  #
  # Returns:
  #   A special object for use in the CacheSolve function to cache matrix inverses.
  
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
  # Takes a MakeCacheMatrix object and calculates the inverse of the matrix 
  # specified in the MakeCacheMatrix object using the solve() function. If the 
  # inverse has has already been calculated (e.g. MakeCacheMatrix cache is not null ) 
  # then the cached value will be returned instead. 
  #
  # Args:
  #   cached: a MakeCacheMatrix object
  #
  # Returns:
  #   The inverse matrix of the matrix spefcified in the MakeCacheMatrix object.
  
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




