## Matrix Inverse and store library v1.0...
## To save compute time we store the inverse of the matrix...
## Every time the content of the matrix changes, the user must call .set(y) with the new matrix
## and the next time the inverse is requested it will again calculate.

## makeCacheMatrix() generates a matrix object and inverse it...
## The inverse is stored for later use, so only the first time it
## is requested is it calculated...

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
            x <<- y
            m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve() gets the inverse of a matrix object, if it is already 
## calculated it will just get the last ansver to save time... 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
            message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
