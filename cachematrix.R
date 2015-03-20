## These functions first cache the inverse of a matrix
## Then they check to see if the inverse is calculated, and if it is return the value from the cache.
## Otherwise, they calculate the inverse of the data and then sets that value in the cache. 

## This function sets the value of the matrix, gets the value of the matrix, then sets the value of the inverse and gets the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}


## This function calculates the inverse of the matrix created with the above function. 
## First checks to see if the inverse has already been calculated. If so, it  gets the inverse from the cache and skips the computation. 
## Otherwise, calculates the inverse of the data and sets the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
