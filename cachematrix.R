##Caching the Inverse of a Matrix

##I'm creating a special "matrix" object that can cache its inverse


makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  set <- function(y) {
    m <<- y
    inv <<- NULL
  }
  get <- function() m
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## I'm computing the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has 
##already been calculated and the matrix has not changed, then the cachesolve should retrieve the 
##inverse from the cache.

cacheSolve <- function(m, ...) {
  inv <- m$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- m$get()
  inv <- solve(data, ...)
  m$setinv(inv)
  inv
}
