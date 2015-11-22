## The following functions enable the caching of the inverse of a square matrix
## Only square solvable matrices are supported


## A simple wrapping function to store a matrix and its inverse
## getter and setter functions are exposed in the returned list

makeCacheMatrix <- function(x = matrix()) {
  # holdig value for inverse matrix
  inv <- NULL
  set <- function(y) {
    # update matrix with new value
    x <<- y
    # set the existing inverse to null
    inv <<- NULL
  }
  get <- function()
    x
  setInverse <- function(inverse)
    inv <<- inverse
  getInverse <- function()
    inv
  # expose inner functions in a returned list
  list(
    set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


## Takes a makeCacheMatrix and returns its inverse
## If the inverse has already been solved a cached copy will be returned

cacheSolve <- function(x, ...) {
  cinv <- x$getInverse()
  # return the cached data if not null
  if (!is.null(cinv)) {
    message("getting cached data")
    return(cinv)
  }
  # no cache - generate the inverse and return
  data <- x$get()
  cinv <- solve(data, ...)
  #cache the inverse
  x$setInverse(cinv)
  cinv
}