## These functions create, invert, cache, and then write out a matrix 
## They represent the work product for the second assignment for R Programming

## makeCacheMatrix creates and inverts the matrix
makeCacheMatrix <- function(x = matrix()) {
## Initialize the variable i and the y function
	inverted <- NULL
	set <- function(y) {
	      ## The <<- operator assigns a value to an object in a different environment
          x <<- y
          inverted <<- NULL
	}	
 ## Define and compute the inverted matrix
	get = function() x
	setinv <- function(inverse) inverted <<- inverse
	getinv <- function() inverted
	list(set=set,
		 get=get,
		 setinv = setinv,
		 getinv = getinv)
 
}	 


## cacheSolve returns the inverted matrix calculated by makeCacheMatrix

cacheSolve <- function(x, ...) {
  invert <- x$getinv()
  if(!is.null(invert)){
    message("Loading the cached matrix")
    return(invert)
  }
 ## Structuring and writing the inverted matrix 
  data <- x$get()
  invert <- solve(data)
  x$setinv(invert)
  invert     
}
