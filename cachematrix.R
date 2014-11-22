## R-file, cachematrix.R
## this is a .R file containing two functions for the Coursera
## R programming Programming Assignment 2.

##  The first function turns a matrix into a list object. 
## Within this list object, the inverse of the matrix can be stored
## (i.e. cached) once computed so that future calls for the inverse
## of this particular matrix can access the inverse and save the 
## need to recompute.


###################################################################
## Function 1: makeCacheMatrix.R
## This function takes as input a matrix M and returns a list
## object which is accessible to the cacheSolve function. 

makeCacheMatrix <- function(M = matrix()) {
  I <- NULL
  set <- function(y) {
    M <<- y
    I <<- NULL
  }
  get <- function() M
  setinverse <- function(solve) I <<- solve
  getinverse <- function() I
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##############################################################
## Function 2: cacheSolve.R
## This function takes as input the list object version of a
## matrix, created by makeCacheMatrix.  (It does not take 
## a regular matrix as its input.)  If the list item inverse is
## null because it hasn't been computed, the matrix inverse will
## be computed via R's solve function.  However, if the inverse
## has been computed then this function retrieves the inverse
## from the list instead of computing it.
##  The assumption is that the matrix used for the list object is
## invertible =).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}

cacheSolve <- function(M, ...) {
  I <- M$getinverse()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- M$get()
  I <- solve(data, ...)
  M$setinverse(I)
  I
}
