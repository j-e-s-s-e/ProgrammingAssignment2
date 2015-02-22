# The 2 functions below are designed to calculate the inverse 
# of a matrix, and then cache that inverse so it does not have
# to be recalculated. This is helpful in instances where in-
# verse matrix calculation is looped.

################################################################

# The function below, makeCacheMatrix, creates a matrix con-
# taining a list of functions that (1)sets the value of the 
# matrix, (2) gets the value of the matrix, (3) sets the value
# of the inverse of the matrix, and (4) gets the value of the
# inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get, setmean = setmean,
		getmean = getmean)
}

# The function below, cacheSolve, calculates the inverse of
# matrices created by the makeCacheMatrix function above.
# This function first checks the cache to see if an inverse has 
# already been calculated for a given matrix, and will only 
# calculate inverse matrices that don't already exist in the cache.

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}
