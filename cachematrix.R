## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix creates a function and storage location to store
## a matrix and it's inverse.  It also provides set and get functions for each.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## cacheSolve attempts to retreive the inverse matrix from the makeCacheMatrix
## object.  If it is successful, it returns the inverse matrix.  If not, it
## calculates the inverse and stores it in the makeCacheMatrix object.  It 
## then returns the inverse matrix.   Only the first call to this function 
## should calculate the inverse.  Subsequent calls should return the previously
## calculated inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
      message("getting cached data")
      return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
