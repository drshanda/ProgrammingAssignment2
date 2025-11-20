## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object that is really a list 
## of four functions:
## 1. set(y): sets the value of the matrix
## 2. get(): gets the value of the matrix
## 3. setInverse(inv): sets the value of the inverse
## 4. getInverse(): gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  # 'inv' will store the cached inverse, initialized to NULL
  inv <- NULL
  
  # Function to set the matrix and reset the cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL # Invalidate the cache when the matrix changes
  }
  
  # Function to get the stored matrix
  get <- function() x
  
  # Function to set the inverse in the cache
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  # Function to get the cached inverse
  getInverse <- function() inv
  
  # Return a list of the four functions
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix
## has not changed), then it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  # Attempt to retrieve the cached inverse
  inv <- x$getInverse()
  
  # Check if the inverse is already cached (not NULL)
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv) # Return the cached inverse
  }
  
  # If the inverse is NOT cached, get the matrix data
  data <- x$get()
  
  # Calculate the inverse of the matrix (using solve())
  inv <- solve(data, ...)
  
  # Cache the calculated inverse using the setInverse function
  x$setInverse(inv)
  
  # Return the inverse
  return(inv)
}
