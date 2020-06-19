## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly

## The following functions are used to create a special object that stores a matrix and caches its inverse. 
## The first function, makeCacheMatrix creates a special “matrix”, which is really a list containing a function to:

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  xi <- NULL
  set <- function(y) {
    x <<- y
    xi <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) xi <<- inverse
  getinverse <- function() xi
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
    xi <- x$getinverse()
  if (!is.null(xi)) {
    message("getting cached data")
    return(xi)
  }
  data <- x$get()
  xi <- solve(data, ...)
  x$setinverse(xi)
  xi
}
