## This is the edited code. Last commit on this code was done on 27-8-2020
## This function creates a special "matrix" object that can cache its inverse.
## smat is the matrix object that user will submit on the console


makeCacheMatrix <- function(smat = matrix()) {
    invsmat <- NULL
    set <- function(x) {
        smat <<- x
        invsmat <<- NULL
    }
    get <- function() smat
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() invsmat
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}
## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(smat, ...) {
    ## Return a matrix that is the inverse of 'smat'
    inv <- smat$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(invsmat)
    }
    mat <- smat$get()
    invsmat <- solve(mat, ...)
    smat$setInverse(invsmat)
    invsmat
}
