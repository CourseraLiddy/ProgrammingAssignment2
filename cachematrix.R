## These functions take the inverse of a matrix, getting the 
## cached copy if one is available instead of recomputing

## Description from assignment, function template from assignment
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Description from assignment, function template from assigment
## This function computes the inverse of the special "matrix" returned by  
## makeCacheMatrix  above. If the inverse has already been calculated 
## (and the matrix has not changed), then  cacheSolve  should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
