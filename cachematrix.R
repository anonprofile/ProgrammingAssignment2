## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than computing it repeatedly.

## The following two functions are used to to create a special object that stores a matrix and
## caches its inverse using the <<- operator to assign a value to an object in an environment
## that is different from the current environment.

## Assumptions: the matrix supplied is always invertible.



## This function creates a special matrix which is a list containing functions to:
##   1. set the value of the matrix
##   2. get the value of the matrix
##   3. set the value of the inverse of the matrix
##   4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  ## Create a special matrix from 'x' that can cache its inverse

  ## initialize the inverse to NULL
  inv <- NULL
  
  ## function to set the matrix, caching the matrix and a NULL inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## function to get the matrix
  get <- function() x
  
  ## function to set (and cache) the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  ## function to get the inverse of the matrix
  getinverse <- function() inv
  
  ## return a list of the special matrix's functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function calculates the inverse of the special "matrix" created with the above makeCacheMatrix function. 
## First, it checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## get the inverse of the matrix
  inv <- x$getinverse()
  
  ## return the cached inverse if it exists
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## get the matrix
  data <- x$get()
  
  ## calculate the inverse of the matrix
  inv <- solve(data, ...)
  
  ## set the inverse of the matrix
  x$setinverse(inv)
  
  ## return the inverse of the matrix
  inv
}

