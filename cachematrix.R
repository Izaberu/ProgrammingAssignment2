## These functions store the inverse of a matrix
## into a cache and return it, or compute and return 
## the inverse of a matrix. 


## The following function creates an object
## and returns a list of functions. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    library(MASS)
    setinverse <- function(ginv) m <<- ginv
    getinverse <- function() m
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}


## The following function returns the cached inverse 
## of a matrix, and if it does not exist, computes 
## and returns the inverse of a matrix. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- ginv(data, ...)
    x$setinverse(m)
    m
}
