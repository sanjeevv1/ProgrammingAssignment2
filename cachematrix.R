## These two functions take a square invertible matrix as input
## and returns the inverse of the input matrix as a result. 
## The inverse result is stored in the cache so that every subsequent time
## the same input matrix is provided, the inverse is not computed
## from scratch, but retrieved from the cache instead.
##
## The first fucntion takes a square invertible matrix as input and
## returns a list of four functions that * Set the value of the input
## matrix * Get the value of the input matrix * Set the value of the
## inverse of the matrix and * get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL		
	set <- function (y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(opposite) inverse <<- opposite
	getinverse <- function() inverse
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)

}


## This function checks to see if the inverse of the input matrix
## is already in the cache. If so, it retrieves it from there.
## If not, it computes and returns the inverse of the input matrix.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
	if (!is.null(inverse)) {
		message("Getting inverse from cache")
		return(inverse)
	}
	else {
		inverse <- solve(x$get())
		x$setinverse(inverse)
		return(inverse)
	}
}