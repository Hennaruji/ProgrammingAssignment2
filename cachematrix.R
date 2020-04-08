## makeCacheMatrix creates a 4 entry list that ultimately stores the inverse of a
## matrix, and cacheSolve retrieves an existing inverse or caculates a new inverse
## and has that inverse cached.

## makeCacheMatrix accepts a matrix and then creates a list of functions
## The first will "set" the matrix x to a new matrix y and reset invers
## to NULL. The second will return ("get") the matrix x. The third will set invers
## to the matrix passed, while the 4th will retrieve this cached inverse. 

makeCacheMatrix <- function(x = matrix()) {
  
  invers <- NULL
  
  set <- function(y){
    x <<- y
    invers <<- NULL
  }
  
  get <-function() x
  
  setInvers <- function(inverseMatrix) invers <<- inverseMatrix
  
  getInvers <- function() invers
  
  list(set = set,
       get = get,
       setInvers = setInvers,
       getInvers = getInvers)
  
}


## CacheSolve retrieves the cached inverse value. If no such value exists,
## the inverse is calculated and then cached.

cacheSolve <- function(x, ...) {
  
  invers <- x$getInvers()
  
  if (!is.null(invers)){
    message("getting cached inverse")
    return(invers)
  }
  
  data <- x$get()
  invers <- solve(data,...)
  x$setInvers(invers)
  invers
  
}