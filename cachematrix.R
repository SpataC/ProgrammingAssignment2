## Put comments here that give an overall description of what your
## functions do

## creates a cache of a given matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
  #1. gets the matrix
  #2. sets the matrix
  #3. sets the inverse of the matrix by solve and caches it
  #4. gets the cached inverse
  
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) cache <<- solve
  getinverse <- function() cache
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## retrieved the cached inverse matrix if computed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cache <- x$getinverse()
  if(!is.null(cache)) {
    message("getting cached data")
    return(cache)
  }
  data <- x$get()
  cache <- solve(data, ...)
  x$setinverse(cache)
  cache
}
