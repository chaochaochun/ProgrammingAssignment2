## This is the assignment for Coursera course R-programming.
## ,containing two functions 

## This funtion create a vector of a list to set the value of the vector, get the value of the vector, 
## then set the value of the mean and get the value of the mean.

makeCacheMatrix <- function(x = matrix()) {
	    i <- NULL
	    set <- function(y) {
	        x <<- y
	        i <<- NULL
	    }
	    get <- function() x
	    setinverse <- function(inv) i <<- inv
	    getinverse <- function() i
	    list(
	        set = set,
	        get = get,
	        setinverse = setinverse,
	        getinverse = getinverse
	    )
	}


## This function calculates the mean of the special "vector" created with the above function.

cacheSolve <- function(x, ...) {
	    i <- x$getinverse()
	    if(!is.null(i)) {
	        message("getting cached data")
	        return(i)
	    }
	    m <- x$get()
	    i <- solve(m, ...)
	    x$setinverse(i)
	    i
	}
