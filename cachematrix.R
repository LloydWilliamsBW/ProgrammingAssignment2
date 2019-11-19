#The following functions create a special object that stores a matrix and caches its inverse.

#The first one, makeCacheMatrix, creates a special matrix which is really a list containing a fn to:
 # 1. set the value of the matrix
 # 2. get the value of the matrix 
 # 3. set the value of the inverse
 # 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
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


#The second one, cacheSolve, valculates the inverse of the matrix created with makeCacheMatrix. 
#However, it first checks to see whether the inverse has already been calculated.
#Is so, it gets the inverse from the cache and skips the computation, otherwise calculates it.

cacheSolve <- function(x, ...) {
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
