## Put comments here that give an overall description of what your
## functions do

## Below two functions to create special  matrix and cache the inverse of matrix

## Write a short comment describing this function

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## Object to hold matrix, initialize the object
  m <- NULL
  
  ## set the values of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## get the values of the matrix
  get <- function() x
  
  ## set the inverse of the matrix
  setinverse <- function(inverse) m <<- inverse
  
  ## get the inverse of the matrix
  getinverse <- function() m
  
  ## list object with methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function

##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ##get the inverse of matrix
  m <- x$getinverse()
  
  ##check, if inverse of matrix in already set to object then return matrix
  
  if(!is.null(m)) {
    message("getting cached data")
    
    ## return inverse of matrix
    return(m)
  }
  
  ##if inverse of matrix is not set to object then get matrix data
  
  data <- x$get()
  m <- solve(data, ...)
  
  ## set inverse of matrix
  x$setinverse(m)
  
  ## return inverse of matrix
  m
  
}