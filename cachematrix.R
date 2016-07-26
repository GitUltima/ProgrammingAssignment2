## Assignment2: Caching the Inverse of a Matrix
## This assignment writes a pair of functions that cache the inverse of a matrix.

## This first function, makeCacheMatrix creates a speacial "Matrix", which is really a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function sees if there is already an inverse present returned by makeCacheMatrix above.
## If the function is present then no computations takes place.
## If not, then the inverese is computed and is set to the value in the cache via setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("pulling cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  message("creating inverse...caching data")
  inv
}