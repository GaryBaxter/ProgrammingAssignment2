## Gary Baxter
## Programming Assignment 2
##
## Included are two functions: makeCacheMatrix and cacheSolve
##
## makeCacheMatrix creates a list of functions (set, get, setinverse, and getinverse)
## that sets the value of a matrix, gets the value of a matrix, sets the inverse of a matrix,
## and gets the inverse of a matrix.
## 
## cacheSolve returns the inverse of a matrix either by calling the solve function or retrieving it
## from a cache if the inverse hasn't been changed
##
## Note: the matrix must be invertible (i.e., a square matrix)
##
##
## makeCacheMatrix (matrix)
##
##
makeCacheMatrix <- function(x = matrix()) {

       	m <- NULL
       
	set <- function (y)  {
        	x <<- y
             	m <<- NULL
       		}
       	
	get <- function () {
		x
		}
       	
	setinverse <- function (inverse) {
		m <<- inverse
       		}

	getinverse <- function () {
		m
       		}

	list (set = set,
		get = get,
              	setinverse = setinverse,
              	getinverse = getinverse)
}


## cacheSolve (matrix)
##

cacheSolve <- function(x, ...) {

	m <- x$getinverse ()

	if (!is.null (m))  {
		message ("getting cached data")
            	return (m)
	}
      
	data <- x$get ()

	m <- solve (data, ...)

	x$setinverse (m)
	
	m

}
