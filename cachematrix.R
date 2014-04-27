## These functions help to reduce time spent computing
## inverse of the matrix when there is a need
## to find the inverse for the same matrix more than one time

## This function creates ant object, which contains matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)}


## This function returns the inverse of the matrix from the object
## ceated by the makeCacheMatrix. If there are no inverse in the object
## mentioned above, then the inverse is computed and returned.

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
