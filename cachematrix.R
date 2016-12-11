## Matrix inversion is usually a costly computation and there may be some benefit 
##to caching the inverse of a matrix rather than compute it repeatedly

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(i) inv <<- i
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been 
##calculated (and the matrix has not changed), then the cachesolve 
##should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    matequal <- function(x, y){
        is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
    }
    
    data <- x$get()
    inv <- x$getinverse()
    ##return the cached inverse if the matrix has not been changed
    ##and inverse is found in cache
    if(!is.null(inv) && matequal(x$,data)) {
        message("getting cached data")
        inv
    }
    else{
        ##assume all the matrix are assume that 
        ##the matrix supplied is always invertible
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
    }
}


