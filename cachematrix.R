## Some operations in R are computationally intensive
## rather than rerunning computationally intensive opeartions repeatedly
##It amy be useful to cash the resutls of the computation for recall later
## Matrix inversion can be computationally intensive and is used as an example here

## This function creates a list object containing functions to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse of the matrix
##get the value sof the inverso fo the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv##
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function creates the inverse of the supplied matrix
##It first checks if the results of this inversion have already been cached
##If it has it returns that inverted matrix
##If not it calculates the inverted matirx and caches it
##and returns the value of the inverted matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
