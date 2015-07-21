
## The assigment is regarding lexical scoping and caching functions that may require a long
## computation time

## The first function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m = NULL # provides a default if cacheSolve has not been used
  setmatrix <- function(y) { # set value of the matrix
    x <<- y # caches the inputted matrix so that cacheSolve can check whether it has changed
    m <<- NULL # sets the value of m, which is the inversed matrix if cacheSolve is used, to NULL
  }
  getmatrix = function() x # sets the value of the matrix
  setinverse = function(inverse) m <<- inverse
  getinverse = function() m
  list(setmatrix=setmatrix,getmatrix=getmatrix, # creates a list to house the four functions
       setinverse = setinverse,
       getinverse=getinverse)
}

## The second function computes the inverse of the "matrix" returned by makeCacheMatrix(). 
## If the inverse has already been calculated and the matrix has not changed, 
## it'll retrieves the inverse from the cache directly.

cacheSolve <- function(x,...) {
  m <- x$getinverse() # returns the inverse of the original matrix input to makeCacheMatrix()
  if(!is.null(m)) { # if the inverse has already been calculated
    message("getting cached data") # get it from the cache and skips the computation.
    return(m)    
  } # otherwise, calculates the inverse 
  y <- x$getmatrix() # run the getmatrix function to get the value of the input matrix
  m = solve(y,...) # compute the value of the inverse of the input matrix
  x$setinverse(m) # run the setinverse function on the inverse to cache the inverse
  return(m) # return the inverse
}











