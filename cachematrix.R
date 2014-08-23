## A pair of functions to calculate and save the
## inverse of a matrix so the same calculation does
## not need to be made a second time.

## makeCacheMatrix returns a vector of functions so 
## all the functions can be accessed externally.
## The functions it contains can be used by cacheSolve
## to store the matrix and the inverse and then
## retrieve them later

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(solve) inv <<- solve
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## cacheSolve uses the functions from makeCacheMatrix
## to solve the inverse, first by retrieving it if the 
## inverse has been calculated before, and then by 
## calculating it de novo

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
   ## Return a matrix that is the inverse of 'x'
}
