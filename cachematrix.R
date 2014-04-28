## This pair of functions creates a special matrix and provides functionality to
## calculate, cache and recover from the cache the inverse of the matrix.

## The makeCacheMatrix funtion creates a special "matrix" which is a list of functions:
## 1. set the values of the matrix
## 2. get the values of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {
            x <<- y
            s <<- NULL
      }
      get <- function() x
      setInverse <- function(solve) s <<- solve
      getInverse <- function() s
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## The cacheSolve function checks to see if the inverse of the special matrix
## has been calculated and cached. If it has the cache'd inverse is returned.
## If it has not it calculates the inverse and caches it using the setInverse
## funtion.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      s <- x$getInverse()
      if(!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      data <- x$get()
      s <- solve(data, ...)
      x$setInverse(s)
      s
}
