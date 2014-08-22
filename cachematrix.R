## The following functions calculate the inverse of a matrix and store it in cache
## so it can be access from there once is computed in order not to waste time in this calculation
## once it has been calculated. 


## This function creates a list that contains the matrix and the inverse of it
## and store it in  cache.

makeCacheMatrix <- function(x = matrix()) { # receive a matrix as input
    s <- NULL # s is the inverse of the matrix and its set to NULL 
	set <- function(y) {
                x <<- y
                s <<- NULL
        }
	get <- function() { x } # function that return the value of the matrix
	setinv <- function(solve) { # function that is called to make the inverse and store the value of it
	    s <<- solve 
		}
	getinv <- function() { s } # function that return the cache value
	list(get = get, setinv= setinv, getinv = getinv)
	}

## This function takes the object created by makeCacheMatrix() and return the inverse
## either from di novo or from cache 

cachesolve <- function(x, ...) { # function that takes an makeCacheMatrix() object and 
                                 # return the inverse of the matrix di novo or from cache
        s <- x$getinv() # gets the value of the inverse 
        if(!is.null(s)) { # if the inverse has been calculated previously then 
                message("getting cached data") # return a message that says that it is retrived from cache
                return(s) 
        }
        data <- x$get() # if the inverse is not in cache then its calculated. First gets the data
        s <- solve(data, ...) # then calculates the inverse
        x$setinv(s) # stores it 
        s # and returns it
}
