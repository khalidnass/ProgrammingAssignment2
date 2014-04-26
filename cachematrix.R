##  Khalid April 26 
##  The objective of this function is to create an object to save cacheSolve result … so that you don’t need 
##  To perform the item every time ….
 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setmInverse <- function(Inverse) inv <<- Inverse
  getInverse  <- function() inv
  list(set = set, get = get,
       setmInverse = setmInverse,
       getInverse  = getInverse)
}
##  Khalid April 26 
##  The objective of this function is Return a matrix that is the inverse of 'x' and stores the result in makeCacheMatrix object

cacheSolve <- function(x, ...) {
 #1) x Matrix cache
  inv <- x$getInverse()       
  
  #2) if there is a cache return inv
  if(!is.null(inv)) {         
    message("getting cached data")
    return(inv)    
  }
  
  #3) if there's no cache solve the matrix
  data <- x$get() #if there's no cache
  inv <- solve(data, ...)
  x$setmInverse(inv)
  
  #4) return the result 
  inv
}
