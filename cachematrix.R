## Put comments here that give an overall description of what your
## functions do

# Sample code:
# > x <- matrix(rnorm(9), nrow = 3)     // Create a matrix x
# > cx <- makeCacheMatrix(x)            // Create a special matrix
# > cx$get()                            // Print the matrix
# > cacheSolve(cx)                      // Return the inverse matrix
# > cacheSolve(cx)                      // Call the 2nd time, so return the cached inverse


## Write a short comment describing this function
# This function creates a special "matrix" object
# that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  invmat <- NULL
  
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  
  get <- function() x
  setinv <- function(inverse) invmat <<- inverse
  getinv <- function() invmat
  
  list(set = set, get = get, 
       setinv = setinv, 
       getinv = getinv)
  
}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve
#the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  invmat <- x$getinv()
  
  if (!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  
  data <- x$get()
  invmat <- solve(data, ...)
  x$setinv(invmat)
  invmat
}