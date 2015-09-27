## Cache inverse matrix

## This solution is based on template provided by assignment description.

## This function creates a matrix but also can store the inverse on cache.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solution) inverse <<- solution
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function solves the inverse of a matrix. But firt it checks if there a
## stored solution in cache. If yes, return the stored solution, else make the
## computation.
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    message("Returning data from cache")
    return(inverse)
  }

  # Here there is no solution on cache. So, lets calculate.
  data <- x$get()
  # Solve the problem
  inverse <- solve(data, ...)
  # Stores the inverse into cache
  x$setinverse(inverse)
  # Return the inverse
  inverse
}
