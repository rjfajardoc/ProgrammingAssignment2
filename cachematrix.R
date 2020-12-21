## Set of functions to store a matrix in the cache, and calculate
## its inverse or retrieve the calculation if it was solved already.

## Create an object to store the matrixm and the calculated
## inverse in the cache. The matrix and inverse are stored as a list
## of functions that 1. Store the matrix data (set). 2. Return the
## matrix (get). 3. Store the inverse matrix data (setinverse).
## and 4. Return the inverse matrix (getinverse). 
makeCacheMatrix <- function(x=matrix()){
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    ##Set the value of the inverse matrix
    setinverse <- function(solve) m <<- solve
    ## Get the value of the inverse matrix
    getinverse <- function() m
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), cachesolve retrieves the inverse
## from the cache.
cacheSolve <- function(z,...){
    n <- z$getinverse()
    if(!is.null(n)){
        message("Getting matrix from cache")
        return(n)
    }
    data <- z$get()
    n <- solve(data, ...)
    z$setinverse(n)
    n
}
