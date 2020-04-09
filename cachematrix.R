## Put comments here that give an overall description of what your
## functions do

## A function to create a matrix and cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## A function to compute inverse of matrix and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting inverse of matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
