## This function will take an inverse of a matrix and save as cache for later use

makeCacheMatrix <- function(x = matrix()) {
  ## This will create the Matrix that you wish to cache, allows it to be available for cache tracking
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheinv <- function(x, ...) {
  ## This function checks to see if a cached value is already present.  
  ## If it is present, it will use that value (and save time!)
  ## If it is not present, it calls the vunction setinv to store the matrix inverse in cache.
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}