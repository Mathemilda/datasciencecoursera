## The function takes a matrix and creates an equal matix with 
## cached attachment. 
## The attachment will be created by another function (cacheSolve)
## when it is called and it is the inverse of original matrix.
##

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inverse <<- solve
  getsolve <- function() inverse
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inverse <- x$getsolve()
  if(!is.null(inverse)) { 
    message("getting cached data")
    return(inverse)
  }
        ## Return a matrix that is the inverse of 'x'
  data <- x$get()
  inverse <- solve(data, ...)
  x$setsolve(inverse)
  inverse
}
