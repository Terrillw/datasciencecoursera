## The makeCacheMatrix function provides a list of functions for a passed matrix variable.
##  Use these functions to print or set the matrix and the inverse of the matrix.
##  The setinverse function puts the result in the parent environment.
## The cacheSolve function returns the inverse of the variable returned from makeCacheMatrix.
##  It returns that inverse using that cached version if available. Otherwise, it calculates
##  the inverse.


## Creates 4 functions to manipulate the passed matrix variable. The setinverse function
##  puts the result in the parent environment.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'. It is assumed that 'x' is the result of
##  the makeCacheMatrix function. It uses the setinverse function within the returned list of 
##  makeCacheMatrix function to output the inverse of the matrix. It only uses that setinverse
##  if the result is not already stored in the parent environment / cache.
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
