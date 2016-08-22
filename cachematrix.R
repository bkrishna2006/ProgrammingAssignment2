## This function creates a special "matrix" object that can cache its inverse.

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
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
#above.If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

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

# function to test makeCacheMatrix and cacheSolve functions on a nXn matrix mat1
set.seed(1110201)
r = rnorm(1000000)
mat1 = matrix(r, nrow=1000, ncol=1000)
temp = makeCacheMatrix(mat1)
start.time = Sys.time()
cacheSolve(temp)
dur = Sys.time() - start.time
print(dur)
start.time = Sys.time()
cacheSolve(temp)
dur = Sys.time() - start.time
print(dur)
