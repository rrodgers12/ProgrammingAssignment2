## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Create a special matrix that will hold and cache attributes including
## the matrix value and its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
			set <- function(y) {
					x <<- y
					inv <<- NULL
			}
			get <- function() x
			setInverse <- function() inv <<- solve(x)
			getInverse <- function() inv
			list(set = set, get = get,
				 setInverse = setInverse,
				 getInverse = getInverse
}


## Write a short comment describing this function
## This function can calculate and set the inverse of a matrix
## but will check if the value already exists to avoid re-calculting
## if the matrix hasn't changed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
