
## makeCacheMatrix() is a function to create a special matrix object that can cache its inverse
## It also assigns a list of functions
## cacheSolve() is a function used to get inverse of matrix returned by makeCacheMatrix above


## makeCacheMatrix() creates an object of type matrix
## Functions of fetching matrix,inverse of matrix and changing values of both matrices 
## as well as inverse of matrix are defined.

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL				# Initializing Inverse of Matrix
  set <- function(y = matrix())	# Changing Matrix object
  {
    if(!identical(x,y))		# Check to compare old and new matrix
    {
      message("Change matrix")
      x <<- y
      inv <<- NULL
    }
    else
    {
      message("Matrix unchanged")
    }
  }
  get <- function() x			# Returning Matrix object
  setinv <- function(i = matrix()) inv <<- i	# Changing Inverse of matrix
  getinv <- function() inv		# Returning Inverse of Matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheinv() returns inverse of matrix object from above
## We do this either by getting value in cache
## or calculating the inverse.

cacheinv <- function(x,...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
