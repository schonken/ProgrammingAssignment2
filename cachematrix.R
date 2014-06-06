## The following functions provide caching capabilities for
## potentially costly matrix solves (inversions)

## Create and return a list of functions for input matrix 'x' providing 
##  - set()
##  - get()
##  - setSolve()
##  - getSolve()
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


## Calculate, cache and return a matrix that is the solve (inverse) of 'x'
cacheSolve <- function(x, ...) {
    s <- x$getSolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    
    message("calculating and creating cached data")
    data <- x$get()
    s <- solve(data, ...)
    x$setSolve(s)
    s
}

## Just a bit of script to test if all is well in the land of sovle caching
# m <- makeCacheMatrix(matrix(1:4, 2, 2))
# cat("\n")
# print(cacheSolve(m))
# cat("\n")
# print(cacheSolve(m))
# cat("\n")
# print(m$get())
# cat("\n")
# print(m$getSolve())
# cat("\n")