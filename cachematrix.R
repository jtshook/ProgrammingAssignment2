## Two functions used to initialize a matrix and calculate it's inverse

## makeCacheMatrix is a function in which you create a special matrix object
## that can chache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                                 # inverse is initially stored as null
  set <- function(y) {
    x <<- y                                 # creates a set function that sets the matrix to y 
    m <<- NULL
  }
  get <- function() x                       # create a get function that returns the matrix
  setsolve <- function(solve) m <<- solve   # set function stores the inverse matrix
  getsolve <- function() m                  # returns the inverse matrix
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  m <- x$getsolve()                   # check if the inverse has been calculated
  if(!is.null(m)) {                   # if the inverse has been calculated, return it
    message("getting cached data")
    return(m)
  }
  data <- x$get()                     # get the underlying matrix data
  m <- solve(data, ...)               # calculate it's inverse
  x$setsolve(m)                       # store the inverse to the matrix object
  m                                   # return the inverse matrix
}
