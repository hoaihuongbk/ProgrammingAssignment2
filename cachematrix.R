## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Use this function for make an invertable matrix can be store the invert matrix to cache
makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) im <<- inverse
        getInverse <- function() im
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## Use this function for invert the matrix using cache. Note: the matrix will be apply by makeCacheMatrix function first.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getInverse()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data,...)
        x$setInverse(im)
        im
}
