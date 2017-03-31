## Below are two functions which are used to 
## create special objects that store a square 
## matrix and cache's its inverse. 

## MakeCacheMatrix is a function which sets the
## value of the matrix, gets the value of the
## matrix, sets the value of the inverse of the
## matrix and gets the value of the inverse of
## the matrix.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y)   {
            x <<-y
            m <<- NULL
      }
      get <-function() x
      setinverse < function(solve) m <<- inversematrix
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## CacheSolve calculates the inverse of the
## matrix created with "makeCacheMatrix", but 
## first checks to see if the inverse has already
## been calculated. If so, it gets the inverse
## from the cache and bypasses the calculation.
## If not, it calculates the inverse of the
## matrix and sets the value of the inverse in
## the cache via the setinverse function.

cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if (!is.null(m)) {
              message("getting cached data")
              return(m)}
      # Return a matrix that is the inverse of 'x'
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
      }
