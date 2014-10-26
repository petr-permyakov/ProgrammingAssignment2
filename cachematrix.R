## Put comments here that give an overall description of what your
## functions do

## MakeCacheMatrix function creates a special "matrix" to cache result of the 
## "solve" function which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)

}


## The following function calculates the mean of the special "matrix" created
## with the above function. However, it first checks to see if the inverse matrix has 
## already been calculated. If so, it gets the inverse matrix from the cache and skips 
## the computation. Otherwise, it calculates inverse matrix of the data and sets 
## it in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    ## If  'm' is in the cache - return cached result
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## get matrix data
    data <- x$get()
    ## apply solve function to matrix
    m <- solve(data, ...)
    ## save inverse matrix to cache in 'x'
    x$setsolve(m)
    m
}
