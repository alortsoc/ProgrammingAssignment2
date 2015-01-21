## Functions used to calculate the inverse of a matrix, caching to achieve that it is calculated only once.
## CacheSolve must be invoked with the result of invoking makeCacheMatrix over a matrix.
##
## A use example:
## m <- matrix(c(2,10,20,3), 2 ,2)
## preparedMatrix <- makeCacheMatrix(m)
## cacheSolve(preparedMatrix)
##
## First invocation to cacheSolve(preparedMatrix) will calculate the inverse of matrix m and cache it, 
## following invocations will return the cached value


## Creates a list of functions that will be the input to calculate the inverse of the matrix passed as
## input parameter
## Functions returned are:
##      set --> set the matrix to use in calculations
##      get --> return the matrix used in calculations
##      setSolve --> set the cached inverse matrix (but it does not calculate it). 
##                     It will be used ONLY by cachedSolve function, and by nobody else
##      getSolve --> return the previously set inverse matrix 
makeCacheMatrix <- function(matrix = matrix()) {
  
    matrixSolve <- NULL
    set <- function(newMatrix) {
        matrix <<- newMatrix
        matrixSolve <<- NULL
    }
    get <- function() {
        matrix 
    }
    setSolve <- function(newSolve) { 
        matrixSolve <<- newSolve 
    }
    getSolve <- function() { 
        matrixSolve 
    }
    list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}



## Calculates the inverse of the matrix stored in the list passed as parameter. If it was previously 
## calculated, it return the stored value without doing any calculation, else it calculates the inverse
## and caches it
##
## The input parameter must be a list created by makeCacheMatrix function, otherwise it will not work
## as expected
cacheSolve <- function(preProcessedMatrix, ...) {
        
    solvedMatrix <- preProcessedMatrix$getSolve()
    if(!is.null(solvedMatrix)) {
        return(solvedMatrix)
    }
    rawMatrix <- preProcessedMatrix$get()
    solvedMatrix <- solve(rawMatrix)
    preProcessedMatrix$setSolve(solvedMatrix)
    solvedMatrix
}
