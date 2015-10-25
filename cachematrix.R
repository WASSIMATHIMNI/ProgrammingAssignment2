## Programming Assignment R Course JH Coursera

## Creates a special matrix object that can cache a matrix and its inverse
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
## If the inverse has already been calculated, it will return the inverse from the cache
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
