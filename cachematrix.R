## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix: This function creates a special 
# square "matrix" object that can cache its inverse matrix.
# cacheSolve: This function computes the inverse of 
# the special "matrix" returned by makeCacheMatrix 
# above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve
# retrieve the inverse from the cache.

## Write a short comment describing this function
# this function creates a special "matrix", 
# which is really a list containing a function to
#
#  set the value of the matrix
#  get the value of the matrix
#  set the value of the inverse matrix
#  get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inv_m <- matrix(list(rep(NULL,nrow(x)*ncol(x))), nrow=nrow(x), ncol=ncol(x))
  set <- function(y) {
      x <<- y
      inv_m <<- matrix(list(rep(NULL,nrow(x)*ncol(x))), nrow=nrow(x), ncol=ncol(x))
  }
  get <- function() x
  setinv_m <- function(z) inv_m <<- z
  getinv_m <- function() inv_m
  list(set = set, get = get, getinvm=getinv_m, setinvm=setinv_m)
}


## Write a short comment describing this function
# This function solves the special "matrix".
# But first, it checks - if the inversed matrix already exists.
# If so, then - it returns the cached data.
# Otherwise - it solves the matrix and pushes this value
# into the cache through the setinvm method
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  im <- x$getinvm()
  if(!is.null(unlist(im))){
     message("getting cached data")
     return(im)
   }
   data <- x$get()
   im <- solve(data, ...)
   x$setinvm(im)
   im
}
