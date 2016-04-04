## Put comments here that give an overall description of what your
## functions do:
## I will write 2 functions the first will read a square Matrix,
## and invert it, and store (cache) the invertion in the parent (global) enviroment
## so if invertion reqested again for Matrix it can be loaded from cache, instead of recalculated


## Write a short comment describing this function
## this function ceates a list where you can get and set the matrix value 
## and it inverse from the global env.

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}


## Write a short comment describing this function....

## returns the inverse of a matrix, first checks to see
## if already calculated, and if it is returns that 
## and skips calclation step

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## using a functin called setinv
  
  ## assumes that the matrix is always invertible.
  
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached inverse")
      return(inv)
    }
    d <- x$get()
    inv <- solve(d)
    x$setinv(inv)
    inv
  
}
