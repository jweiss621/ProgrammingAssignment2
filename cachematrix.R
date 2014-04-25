## The purpoose of the following functions is to cache the contents of a
## matrix and the inverse of the matrix so when we need the inverse of the matrix again,
## it can be looked up in the cache rather than recomputed

## The first function makeCacheMatrix creates a special "matrix" which is really a list
## containing a function to:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the inverse of the matrix
## 4) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinv <- function(solve) s <<- solve
  getinv <- function() s
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The second function cacheSolve calculates the inverse of the special matrix created above.
## However, it first checks to see if the inserve has already been calculated. If so, it gets the
## inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the
## matrix and sets the inverse of the matrix in the cache via the setinv function

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  s <- x$getinv()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinv(s)
  s
}
