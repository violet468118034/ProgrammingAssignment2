## The whole function will cache an inversed matrix. 

## This function is used to inverse a matrix, but the result won't show at the directory

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	set_inverse <- function(solve) inv <<- solve
	get_inverse <- function() inv
	list(set = set, get = get,set_inverse = set_inverse, get_inverse = get_inverse)
}


## This function will check whether the matrix has been inversed at first. 
## Then it will skip the compute or inverse the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$get_inverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$set_inverse(inv)
	inv
}