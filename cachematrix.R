## These two functions work in conjunction to cache inverse matrices
## since inversing a matrix can be expensive computationally.

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {
	inverse <- NULL
	set <- function(y) {
		m <<- y
		inverse <<- NULL
	}
	get <- function() m
	setinverse <- function(i) inverse <<- i
	getinverse <- function() inverse
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## Computes the inverse of the special matrix object (list) created by 
## makeCacheMatrix.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
