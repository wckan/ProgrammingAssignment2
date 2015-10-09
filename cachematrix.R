## File created on Oct 8 2015 by Wai-Chi Kan

## These functions can set, get, and inverse a matrix as the same time check if a inverse matrix exists in a cache
## if the inverse matrix exists, it will fetch the inverse matric in the cache instead of calculate it again

## makeCacheMatrix can set a matrix, get the matrix stored, 
## set its inverse matrix and get its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, 
       getinverse = getinverse)
}


## cacheSolve function uses to check if the inverse matrix of 'x'existed   
## if not, it calulates the inverse matrix; otherwise, it fetches from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
