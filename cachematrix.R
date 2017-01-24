## This function creates a matrix object that can cache its inverse. 
## It returns a list of functions: 
##	1. set the matrix
##	2. get the matrix
##	3. set the inverse
##	4. get the inverse
## It works as an input for cacheSolve function
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	
	## set the matrix
	setMat <- function(y) {
		x <<- y
		inv <<- NULL
	}
	
	## get the matrix
	getMat <- function() x
	
	## set the inverse
	setInv <- function(inverse) inv <<- inverse
	
	## get the inverse
	getInv <- function() inv
	
	## result
	list(setMat=setMat, getMat=getMat, setInv=setInv, getInv=getInv)
}


## This function return the inverse of the original matrix. 
## It works with the makeCacheMatrix function
cacheSolve <- function(x, ...) {
        ## initiating the inverse
        inv <- x$getInv()
        
        ## if the inverse has already been calculated
        if (!is.null(inv)) {
        	message("getting cached data")
        	return(inv)
        }
        ## otherwise compute the inverse
        else {
        	inv <- solve(x$getMat(), ...)
        }
        
        ## result
        x$setInv(inv)
        inv
}
