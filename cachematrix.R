## Caching the Inverse of a Matrix

## makeCacheMatrix creates a special matrix, 
##which is really a list containing a function to

#set the value of the matrix
#get the value of the matrix
#set the value of the inverse of a matrix
#get the value of the inverse of a matrix

#If you set the matrix in makeCacheMatrix, the cached inverse is initialized to NULL.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## cacheSolve get the inverse from the cache of the above function. 
## If it is NULL, the inverse is computed and the value is set in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}
