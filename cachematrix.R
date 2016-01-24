## makeCacheMatrix creates a special matrix object
## then cacheSolve uses said matrix to return the inverse.

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setmat <- function(solve) invmat <<- solve
  getmat <- function() invmat
  list(set = set, get = get,
       setmat = setmat,
       getmat = getmat)
}


##cacheSolve returns the inverse of our matrix created with the "makeCacheMatrix".
##It takes the output of makeCacheMatrix as it's input.
##If the cached inverse exists, it is returned without computation.
##If not cached it it will be computed, chached and returned.

cacheSolve <- function(x, ...) {
  invmat <- x$getmat()
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  data <- x$get()
  invmat <- solve(data, ...)
  x$setmat(invmat)
  invmat
}