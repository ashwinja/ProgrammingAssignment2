## makeCacheMatrix:returns a special "matrix" object which holds the passed in matrix and its inverse
## cacheSolve:Takes as argument the  special "matrix" created by makeCacheMatrix and computes and returns 

## This function creates a special "matrix" object, which is really a 
##  list containing a function to
##   1.  set the value of the matrix and set the inverse to NULL
##   2.  get the value of the matrix
##   3.  set the value of the inverse of matrix
##   4.  get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      #if matrix  changes then set inverse to null as it needs to be recalculated
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)  
 
}


## The following function calculates the inverse of the  special "matrix"
#returned by makeCacheMatrix above. However, it first checks to see if the
#inverse has already been calculated. If so, it `get`s the inverse from the
#cache and skips the computation. Otherwise, it calculates the inverse of
#the matrix and sets the value of the inverse in the cache via the `setinverse`
#function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data for inverse")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}  



## Test data used to verify.
# Paste each line below one by one after removing comments into console 
#matrixTest1=matrix(c(1,2,1,1), nrow=2,ncol=2,byrow=TRUE)
#invertibleMatrixObject1 =makeCacheMatrix(matrixTest1)
#first call below will calcuate matrix and cache it
#cacheSolve(invertibleMatrixObject1)
#second call below will get the inverse from the cache as it has allready been calculated
#cacheSolve(invertibleMatrixObject1)
## Change the matrix 
#matrixTest2=matrix(c(1,2,2,1), nrow=2,ncol=2,byrow=TRUE)
#invertibleMatrixObject1$set(matrixTest2)
#Since the matrix has changed first call to cacheSolve will calcuate inverse of matrix and also cache it.
#cacheSolve(invertibleMatrixObject1)
#second call below will get the inverse of matrix from the cache.
#cacheSolve(invertibleMatrixObject1)

