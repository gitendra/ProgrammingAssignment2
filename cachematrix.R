##  makeCacheMatrix Function Description:
##  makeCacheMatrix takes calinverse matrix and calculate the inverse of that. 
##  makeCacheMatrix function defines four functions (listed below) namely "set(setcalinverse)", "get()", "setinverse(inverse)", & "getinverse()" and return them as list from this makeCacheMatrix function.
##        "get()" simply return calinverse which was provided as argument to makeCacheMatrix3.
##        "setinverse(inverse)" simply assigns the inverse value to parent environment variable invmatrix.
##	  "set()" function assigns and argument to calinverse and initialize that. It also set invmatrix to null.
##        "getinverse()" simply returns the parent environment variable invmatrix.

makeCacheMatrix <- function(calinverse = matrix()) {
 ## Return a list containing four function namely "set(setcalinverse)", "get()", "setinverse(inverse)", & "getinverse()"
    invmatrix <- NULL
    set <- function(setcalinverse) {
        calinverse <<- setcalinverse
        invmatrix <<- NULL
    }
    get <- function() calinverse
    setinverse <- function(inverse) invmatrix <<- inverse
    getinverse <- function() invmatrix
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}



## cacheSolve Function Description:
##  such as the list produced by makeCacheMatrix(myMatrix), and produces the matrix inverse (here label
## This function return a matrix that is the inverse of 'calinverse'.
##  cacheSolve takes as its argument a list of fuctions returned from makeCacheMatrix. 
## it first check whether the cached value is availale for the provided argument. If available then siply retrieved and returned.
## Otherwise it retrives the matrix value associated with argument calinverse and determine the inverse matrix of that using solve function.
## After calculating inverse associate that with calinverse and return the same.

cacheSolve <- function(calinverse, ...) {
 ## Return a matrix that is the inverse of 'calinverse'
    matrixinv <- calinverse$getinverse()
    if(!is.null(matrixinv)) {
        return(matrixinv)
    }
    data <- calinverse$get()
    invmatrixval <- solve(data, ...)
    calinverse$setinverse(invmatrixval)
    invmatrixval
}