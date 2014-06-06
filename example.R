## This script implements the example functions illustrating the 
## use of the "<<-" operator

makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}

cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    message("calculating and creating cached data")
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}

## Just a bit of script to test if all is well in the land of mean caching
v <- makeVector(44:63)
cat("\n")
print(cachemean(v))
cat("\n")
print(cachemean(v))
cat("\n")
print(mtrx$get())
cat("\n")
print(mtrx$getmean())
cat("\n")