## makeCacheMatrix and cacheSolve are functions that 
## cache inverse of matrix and retreive it when needed.

## makeCacheMatrix creates an object that houses set, get, 
## setinverse, getinverse functions that can be retreived later. 
## It calculates an inverse of a matrix and stores it in cache.

makeCacheMatrix <- function(x = matrix()) {
  ##initiate inverse
  m <- NULL
  ## create function to set matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## create function to get matrix
  get <- function() x
  ## create function to set inverse
  setinverse <- function(solve) m <<- solve
  ## create function to get inverse
  getinverse <- function() m
  ## create object to store all functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve gets object from the above function, determines 
## if the cache has inverse of the matrix, and either gets it, 
## or calculates it from scratch if it's missing.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  ## if "getinverse" function returns an inverse, return that inverse
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  } 
  ##otherwise get the matrix and calculate the inverse
  else {
  data <- x$get()
  m <- solve(data, ...)
  ##set inverse into the object
  x$setinverse(m)
  m
  }
}