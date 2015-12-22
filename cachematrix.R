## Caching the Inverse of a Matrix


## This function creates a special "matrix" object that can cache its inverse.
## get(): a function that returns the matrix x stored in the main function.
## set(): a function that changes the matrix. 
## setinv() and getinv() don't calculate the inverse, they store the input matrix in a
## variable inv into the main function makeCacheMatrix and return int(getinv).

makeCacheMatrix <- function(x = matrix()) {
	get <- function() x
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix
## has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)){
        	message("getting cached data")
        	return (inv)
        }
        data <- x$get()
        inv <- solve(x)
        x$setinv(inv)
        inv
}
