## These functions create a special matrix object
## that is stored in chached memory. This object also includes
## the inverse of the given matrix.  The inverse is similarly
## stored in cached memory.

## makeCacheMatrix takes a matrix and stores it in the cache for
## easy retrieval, and also creates an inverse variable,
## which is initially empty.  It then defines get functions that
## retrieve the matrix and inverse, respectively, and set functions
## that allows user to set the values of each, and store the new
## values in the cache.

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y){
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(i) inverse <<- i
	getinverse <- function() inverse
	list(set = set, get=get, setinverse = setinverse, getinverse=getinverse)
}


## cacheSolve returns the inverse of the matrix passed to it.
## It will first check if the inverse has already been computed, and
## return the value in the cache if the inverse is already there.
## otherwise, it performs the inverse calculation itself, and sets
## the value in the cache to match.
## PRECONDITION:  The matrix passed is an invertible square matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if(!is.null(i)){
		return(i)
	}
	mat <- x$get()
	i <- solve(mat)
	x$setinverse(i)
	i
}
