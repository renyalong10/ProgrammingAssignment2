## The first function, 'makeCacheMatrix' creates a special "matrix", which is 
## really a list containing a functions to

##1. Set a matrix
##2. Get the matrix 
##3. set a inverse matrix
##4. get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
		In <- NULL
	set <- function(y) {
			x <<- y
			In <<- NULL
	}
	get <- function() x
	setInverse <- function(Inverse) In <<- Inverse
	getInverse <- function() In
	list(set = set, get = get, setInverse = setInverse,
		getInverse = getInverse)
}


## The second function, 'cacheSolve' computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    Inverse <- x$getInverse()
    if(!is.null(Inverse)) {
        return(Inverse)
    }
	data <- x$get()
	Inverse <- solve(data)
	x$setInverse(Inverse)
	x$getInverse()  
}
