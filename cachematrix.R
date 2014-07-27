# Name: cachematrix.R
# Date: July 26, 2014
# Author: Phillip Burger (based significantly on example by RDPeng)
# Purpose: Create a special matrix object and cache the invserse of this object
# Precondition: Matrix is square
# Parameters: x - a matrix
# Notes:
# 1. Instantiate new object example:  a <- makeCacheMatrix()
# 2. Matrix that solves: c <- matrix(c(1, 0, 1, 1, 1, 1, 1, 2 , 2), nrow=3, ncol=3)

# makeCacheMatrix
# Create the special object; solve the matrix via call to cacheSolve()
# Precondition: Matrix is square. setinverse() is used to communicate between
#   object and methods.
# Postcondition: None.
# Parameters: x - an object of class matrix.
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

# cacheSolve
# Solve the matrix via call to cacheSolve()
# Precondition: Special matrix object already exists.
# Postcondition: None.
# Parameters: x - an object of type makeCacheMatrix.
cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached inverse")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}