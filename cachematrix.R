## The following functions creates a special object that stores in a matrix and caches the inverse of the matrix.

## First function creates a matrix. It sets and gets the value, and sets and gets the inverse.

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
  set <- function(y)
   {
    x <<- y
    m <<- NULL
   }

  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function: if the inverse has already been calculated, it gets the inverse from the cache and skips the computation. 
##If not, it computes the inverse and sets the value in the cache.

cacheSolve <- function(x, ...) {
m <- x$getinverse()
  if(!is.null(m))
    {
     message("getting cached data")
     return(m)
    }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
  
}
