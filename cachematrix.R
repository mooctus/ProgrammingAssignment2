## Put comments here that give an overall description of what your
## functions do

## The functions below can be used to compute the inverse of a matrix. 
## As the inverse operation in general can be resource-consuming, we 
## cache the inverse of a matrix once it's been computed for the first
## time. This can be done using closures in R.

## Write a short comment describing this function

## This function has a matrix in input and returns a list with functions
## to set the new value, to get it, to set the inverse and to get the inverse
makeCacheMatrix <- function(x = matrix()){
  
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function

## This function has in input a list built with makeCacheMatrix and computes
## the inverse of the underlying matrix. If the inverse for that matrix has 
## already been computed, then the cached value is returned.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
