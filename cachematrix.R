## Cache the inverse of a matrix for fast computing
## 

## Creates a matrix object that can cache the inverse

makeCacheMatrix <- function(x = matrix()) {
	inv = NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set=set, get=get,setInverse=setInverse,getInverse=getInverse)
}


## Computes the inverse of a matrix returned by makeCacheMatrix function. If the inverse is already calculated
## the function should take the value from cache.

cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
    if (!is.null(inv))
    {
        message("getting cache data")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix)
    x$setInverse(inv)
	## Return a matrix that is the inverse of 'x'
    return(inv)
}
