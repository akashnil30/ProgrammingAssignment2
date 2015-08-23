# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    # holds the cached value or NULL if nothing is cached
    # initially nothing is cached so set it to NULL
    inv <- NULL
     # store a matrix
    set <- function(y) {
        x <<- y
        # since the matrix is assigned a new value, flush the cache
        inv <<- NULL
    }
    # returns the stored matrix
    get <- function() x
    # cache the given argument 
    setinverse <- function(inverse) inv <<- inverse
     # get the cached value
    getinverse <- function() inv
     # return a list. Each named element of the list is a function
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
       
# The following function calculates the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
    # get the catched inverse
    inv <- x$getinverse()
    # if a catched value exists return it
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    # otherwise get the matrix, calculate the inverse and store it in
    # the catche
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    # return the inverse
    inv
}

## Sample run:
## > x = rbind(c(1, -1/4), c(-1/4, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## No cache in the first run
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > 