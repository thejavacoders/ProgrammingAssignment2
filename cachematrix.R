## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix - constructs a list of functions which set/get the data as well as set/get the inverse
##				   - if the inverse is in the cache, it is returned else , inverse is calculated and returned
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
		
		## set the data object in cache
		set <- function(y) {
                x <<- y
                m <<- NULL
        }
		## get the data
        get <- function() x
		
		## populate the cache with the inverse
        setinverse <- function(inv) m <<- inv
		
		## return the inverse
        getinverse <- function() m
		
		## set the list with the getter and setter functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## Write a short comment describing this function
## usage - cacheSolve(makeCacheMatrix(<invertible matrix>))
## 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	
		## get the inverse by invoking it for the special object created using makeCacheMatrix
		m <- x$getinverse()
		
		## if the inverse is cached it is returned else null is returned
        if(!is.null(m)) {
                message("getting cached data")
				
                return(m)
        }
		
		## get the data
        data <- x$get()
		
		## calculate the inverse using solve method provided by R
		##  print("Lets solve")
        m <- solve(data, ...)
		
		## set the inverse in the cache
        x$setinverse(m)
        m
}
