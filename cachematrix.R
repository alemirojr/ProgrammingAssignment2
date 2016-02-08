## These functions are used to cache a given invertible matrix as well 
## as it's inverse matrix
## If the matrix isn't cached yet, stores it in memory, calculate it's 
## inverse, stores it in memory as well, and return it's inverse.
## If it's cached, return its cached inverse


## This function caches the given invertible matrix and responds to 
## caching or getting matrix and its inverse's functions

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get, setinverse = setinverse,
	getinverse = getinverse)

}


## Tests if the matrix is already cached and returns its inverse.
## If not, calls the functions to cache the matrix, it's inverse and
## returns it's inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()	
	if(!is.null(m)) {
		message("getting cached matrix")
		return(m)
	}
	matriz = x$get()
	m <- solve(matriz)
	x$setinverse(m)
	m
}
