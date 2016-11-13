## Matrix inverse is a bit resource consuming 
## so we create cache function to save the inverse matrix
## sample run: 
## > aa <- matrix(data = 1:4, nrow = 2, ncol = 2)
## > bb <- makeCacheMatrix(aa)
## > cc <- cacheSolve(bb)
## re run the cacheSolve will return the cached object
## to reset the matrix with another value and delete the cache:
## > bb$set(aa)

## Create matrix obj that can cache the inverse
## return list contain the cached objects
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

## Compute inverse and push to cache
## the second time this is run, it'll be taken fporm the cache
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
