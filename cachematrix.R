## These functions allow the inverse of a matrix to be cached upon initial calculation.
## Subsequent calls will retrieve the cached inverse.
## Usage:   mtx <- makeCacheMatrix(matrix(c(1,2,3,4),2,2)) creates special object to store and retrieve cached inverse
##          The first call to cacheSolve(mtx) will calculate the inverse and store it in the mtx object.  
##          Additional calls to cacheSolve(mtx) will retrieve the cached inverse rather than re-calculating.

## Create special object to store and retrieve matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(setinv) inv <<- setinv
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}


## Retrieve inverse of a matrix from cache if available.  If not already in cache, calculate inverse and store in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

