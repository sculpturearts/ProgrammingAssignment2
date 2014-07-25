## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setmatrix <- function(mean) m <<- mean
	getmatrix <- function() m
	list(set = set, get = get,
		setmatrix = setmatrix,
		getmatrix = getmatrix)
}

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
	m <- x$getmatrix()
	if(!is.null(m)) {
		message("getting cached inverse")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setmatrix(m)
	m
}
