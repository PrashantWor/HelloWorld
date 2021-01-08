
## Week 3 Assignment on "Cache Inverse Matrix"
# the first function makeCacheMatrix, will first get a matrix
# will check the presence of inverse in the parent and other environments
# will then create a list of all the functions created in this function

makeCacheMatrix<- function(x = matrix()) {
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) Inv <<- inverse
  getinverse <- function() {Inv}
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve() function is in continuation to the makeCacheMatrix() function.
## computes the inverse of the matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  Inv <- x$getinverse()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  mat_data <- x$get()
  Inv <- solve(mat_data, ...)
  x$setinverse(Inv)
  Inv
}



