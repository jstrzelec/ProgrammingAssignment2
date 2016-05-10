
## 
## create a special object to store a numeric square
## matrix and its (ideally for most cases cached) inverse
## 

## 
## matrix object with create,get,set functions
## plus get,set for inverse
## 
makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    set <- function(y) {
      x <<- y
      minv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) minv <<- solve
    getinv <- function() minv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

##
## when inverse is requested,
## if inverse has not already been computed, compute it
## and store it; if it already has been computed, 
## simply return the cached inverse
## 
cacheSolve <- function(x, ...) {
  minv <- x$getinv()
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data, ...)
  x$setinv(minv)
  minv
}
