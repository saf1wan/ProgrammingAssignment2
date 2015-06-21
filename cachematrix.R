## Put comments here that give an overall description of what your
## functions do
##This assignment is to write a pair of functions that cache the inverse of a matrix.

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache and inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
##This function computes the inverse of the special "matrix" returned by the function makeCacheMatrix above 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

##> 
##  > source('~/R Prog/ProgrammingAssignment2/cachematrix.R')
##> x<-matrix(1:4,2)
##> x
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> b = makeCacheMatrix(x)
##> b$get()
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> cacheSolve(b)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> cacheSolve(b)
##getting cached data
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> 
9b0f4a0d20c12faf521ce740303246909f7d87e0
