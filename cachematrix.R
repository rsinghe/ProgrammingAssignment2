## Put comments here that give an overall description of what your
## functions that caches the inverse of matrix

## The function creates a special matrix object that caches it's matrix
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL ##Initialize inverse variable
	
	## Set the matrix
	set <- function(y) {
		matrix <<- y
		inv <<- NULL
	}
	## Get the matrix
	get <- function() {
		matrix
	}
	## Set the inverse of matrix
	setInverse <- function(inverse) {
		inv <<- inverse
	}
	## Get the inverse of matrix
	getInverse <- function() {
		inv // return inverse
	}
	
	## Return the list of methods
	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse}	
}


## This function calculates the inverse of matrix returned by above created function
## If the inverse if already calculated then following function will get the value from cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getInverse()
		
		## Check if inverse is already calculated and return inverse data
		if(!is.null(inv)) {
			return(inv)
		}
		
		## Following code will be evaluated only if inverse data is not cached
		## i.e. if is.null(inv) is NULL
		data <- x$get()
		
		## Calculating inverse
		m <- solve(data) %*% data
		
		x%setInverse(m)
		
		## Returning the matrix that is inverse of 'x'
		m		
}
