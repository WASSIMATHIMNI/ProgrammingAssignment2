## Creates a special matrix object that can cache the inverse of the matrix given
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL

    set <- function(y){
        x <<- y
        inverse <<- null
    }
    get <- function() x
    setInverse <- function(x) inverse <<- x
    getInverse <- function() inverse

    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Computes the inverse matrix of the matrix object in the cache
## If the inverse has already been calculated, will return the inverse from the cache
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()

    if(!is.null(inverse)) inverse
    else{
        matrix <- x$get()
        inverse <- solve(matrix)
        x$setInverse(inverse)

        inverse
    }
}
