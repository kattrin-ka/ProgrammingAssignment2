## makeCacheMatrix creates a special "matrix" object 
##that can cache its inverse
## cacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. 
##If the inverse has already been calculated 
##(and the matrix has not changed), then cacheSolve should 
##retrieve the inverse from the cache.
## creates a special "matrix" object

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setCache <- function(solve) m <<- solve
  getCache <- function() m
  list(set = set, get = get,
       setCache = setCache,
       getCache = getCache)
}


## computes the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getCache()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setCache(m)
  m
}
