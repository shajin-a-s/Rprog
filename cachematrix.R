makeCacheMatrix <- function( Mx = matrix() ) {

	Inv <- NULL

	set <- function( matrix ) {
		Mx <<- matrix
		Inv <<- NULL
	}

	get <- function() Mx

	setInverse <- function(inverse) Inv <<- inverse

	getInverse <- function() Inv

	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}

