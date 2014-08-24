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
cacheSolve <- function(x, ...) {

	Mx <- x$getInverse()

	if( !is.null(Mx) ) {
		message("Got cached Matrix")
		message("Don't Inverse")
		return(Mx)
	}

	d <- x$get()

	Mx <- solve(d) %*% d

	x$setInverse(Mx)

	Mx
}

