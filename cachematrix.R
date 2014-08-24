## makeCacheMatrix : This function creates a special "matrix"
## 		     object that can cache its inverse.
makeCacheMatrix <- function( Mx = matrix() ) {

	## Initialize the inverse matrix to NULL
	Inv <- NULL

	## Set Function to set the matrix
	set <- function( matrix ) {
		Mx <<- matrix
		Inv <<- NULL
	}

	## Function to get the Matrix
	get <- function() Mx

	## Function to set the Inverse of a Matrix
	setInverse <- function(inverse) Inv <<- inverse

	## Function to get the Inverse of a Matrix
	getInverse <- function() Inv

	## Return a list
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}

## CacheSolve: This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

	## Get the Matrix & Invert it
	Mx <- x$getInverse()

	## Return the Inverse if set already
	if( !is.null(Mx) ) {
		message("Got cached Matrix")
		message("Don't Inverse")
		return(Mx)
	}

	d <- x$get()

	## matrix Multiplication
	Mx <- solve(d) %*% d

	x$setInverse(Mx)

	## Return the Matrix
	Mx
}

