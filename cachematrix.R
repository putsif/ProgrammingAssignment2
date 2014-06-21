## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix ceates an object to store an invertible matrix 
## and cache it's inverse. The inverse is initialized to NULL.
## Run cacheSolve on the object to calculate and set the actual inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse<- NULL
  set <-function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse<- function(solve) inverse <<- solve
  getInverse <- function() inverse
  list( set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## cacheSolve calculates and sets the cached value of a makeCacheMatrix object
## using the solve() function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse<- x$getInverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
