## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ivar <- NULL
  set <- function(yvar) {
    x <<- yvar
    ivar <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) ivar <<- inv
  getinverse <- function() ivar
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
  
}

cacheSolve <- function(x, ...) {
  ivar <- x$getinverse()
  if(!is.null(ivar)) {
    message("getting cached data")
    return(ivar)
  }
  mvar <- x$get()
  ivar <- solve(mvar, ...)
  x$setinverse(ivar)
  ivar
  }
