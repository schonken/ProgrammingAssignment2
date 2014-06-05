## The following functions provide caching capabilities for
## potentially costly matrix inversions

## Cache and return a list for input matrix 'x' providing functions
##  - set()
##  - get()
##  - setsolve()
##  - getinv()
makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Calculate, cache and return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
        return(s)
    }
    
    #message("cache is empty (i.e. s is null)")
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}

## Just a bit of script to test if all is well in the land on sovle caching
# mtrx <- makeCacheMatrix(matrix(1:4, 2, 2))
# cacheSolve(mtrx)
# print(mtrx$get())
# print(mtrx$getsolve())
# print(mtrx$getsolve())
