##Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix 
##rather than compute it repeatedly. 
##We try to write here a pair of functions that cache the inverse of a matrix.

##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  matrix <- NULL
  set <- function(y) {
    x <<- y
    matrix <<- NULL
  }
  get <- function() x
  setinv <- function(solve) matrix <<- solve
  getinv <- function() matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  matrix     
}


##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
##we assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matrix <- x$getinv()
  if(!is.null(matrix)) {
    message("getting cached data")
    return(matrix)
  }
  data <- x$get()
  matrix <- solve(data, ...)
  x$setinv(matrix)
  matrix
}
