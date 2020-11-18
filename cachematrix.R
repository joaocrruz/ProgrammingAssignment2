## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function: 
## The first function, makeCacheMatrix creates a special "vector", 
## which is really a list containing a function to:
## 1 - set the value of the matrix, 2 - get the value of the matrix, 
## 3 - set the value of the inverse of the matrix,
## 4 - get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function: 
## The following function calculates the inverse of matrix created
## with the above function. However, it first checks to see if the 
## inverse of the matrix has already been calculated. If so, it gets the 
## result from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the matrix and sets its value in the cache via the
## setInverse function.

cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
