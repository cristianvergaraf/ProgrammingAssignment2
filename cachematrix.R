## These functions are use to save time when we need to perform heavy computations, in this case the computation of the
## inverse of a matrix. It is very useful when in a program the inverse value of a matrix x is use several times. 
## Therefore we can save time and allow our program to run faster.

## MakeCacheMatrix takes a matrix as input and creates a special matrix in which the inverse value of the matrix can be calculated and 
## kept in the cache.
## It is related to the cacheSolve function, which actually calculates the inverse function, through the setsolve and getsolve functions.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
  
}


## In the first part cacheSolve checks if the value of the inverse matrix was previewsly calculated. If it was, returns this value. 
## If it was not, it calculates the value and return it. 

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m  
        
}
