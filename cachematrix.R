## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
# The first 2 lines of code initiates "x" and "invm" as objects, both giving them default values of null. "Set" then sets the values of the vectors for objects "x" and "invm". The <<- operator ensures that any previous values the 2 objects had will be removed and replaced for new ones.
## Overall the makeCacheMatrix ensures that the inverse of the matrix(x) can be stored. 


makeCacheMatrix <- function(x = matrix()) {
	       invm <- NULL
	       
	       set <- function(y){
	       	x <<- y
	       	invm <<- NULL
	       	}
	       	
	       	get <- function() x
	       
	       	setinvm <- function(solve) invm <<- solve

	        getinvm <- function() invm
	        
	        list(set=set, get=get, setinvm=setinvm, getinvm=getinvm)
}


## Write a short comment describing this function. 
##The cachesolve function will actually return the inverse of the matrix x, which will have been stored in the makeCacheMatrix function. If inverse is already previously calculated it will return a text saying "getting cached data". If invm is not previously calculate it will recompute calculation to again find the new inverse of matrix x.  


cacheSolve <- function(x, ...) {
	
	invm <- x$getinvm()
	
    if(!is.null(invm)){
    	message("getting cached data")
    	return(invm)
    	}
    	
    	data <- x$get()
    	invm <- solve(data, ...)
    	x$setinvm(invm)
    	invm
	
	}
        ## Return a matrix that is the inverse of 'x'


aMatrix <- makeCacheMatrix(matrix(1:4, 2, 2)) ## This is an intermediary function needed for the cacheSolve() function to work.

cacheSolve(aMatrix) ## This should return the inverse of aMatrix. 



