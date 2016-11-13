## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Create matrix obj that can cache the inverse
makeCacheMatrix <- function(x = matrix()) {
    inverseMtx <- NULL
    set <- function(x) {
        mtx <<- x
        inverseMtx <<- NULL
    }
    get <- function() mtx
    setInverse <- function(inv) inverseMtx <<- inv
    getInverse <- function() inverseMtx
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## Compute inverse and push to cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached matrix")
        return(inv)
    }
    dataMtx <- x$get()
    inv <- solve(dataMtx, ...)
    x$setInverse(inv)
    inv
}
