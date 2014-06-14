## The functions makeCacheMatrix and cacheSolve
## allow one-time calculation and easy retrieval
## of matrix inverse.

## Example:

## > m = makeCacheMatrix(matrix(1:4, 2, 2))
## > cacheSolve(m)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(m)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## ATTENTION!
##
## Additional arguments in cacheSolve (such as b and tol)
## can lead to unexpected results. Essentialy, their values
## are used only in the first call to cacheSolve and
## ignored in the subsequent calls. This is minor flaw of
## our simple caching scheme.


## Creates a special "cache matrix" object that
## can cache the inverse of given matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) s <<- solve
        getSolve <- function() s
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## Returns a matrix that is the inverse of the
## "cache matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated,
## returns inverse from the cache

cacheSolve <- function(x, ...) {
        s <- x$getSolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setSolve(s)
        s
}
