# Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse
# of a matrix rather than compute it repeatedly. This following assignment is to write a pair of 
# functions that cache the inverse of a matrix.

# The "<<-" operator which can be used to assign a value to an object in an environment that is different
# from the current environment.

# The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a 
# function to:
# set the value of the vector
# get the value of the vector
# set the value of the mean
# get the value of the mean

makeCacheMatrix <- function(x = matrix())
{
	s <- NULL
	set <- function(y)
	{
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) s <<- solve
	getsolve <- function() s
	list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


# The following function calculates the inverse of the special "matrix" created with the above function. 
# However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse 
# from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the 
# value of the inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...)
{
	s <- x$getsolve()
	if(!is.null(s))
	{
		message("getting cached inverse matrix")
		return(s)
	}
	s <- solve(x$get())
	x$setsolve(s)
	return(s)
}


## Sample run:
## > x <- matrix(c(2,4,3,5), nrow = 2)          // Create a matrix x
## > mx <- makeCacheMatrix(x)			// Create our matrix mx
## > mx$get()					// Return the matrix
## > cacheSolve(mx)                             // Return the inverse
## > cacheSolve(mx)                             // Retrieving from the cache in the second run
