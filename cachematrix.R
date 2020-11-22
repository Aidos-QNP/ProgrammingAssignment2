## Goal is cache the inverse of a matrix.
## It works for square and invertible matrix. For example, A is a matrix.
##speA<-makeCacheMatrix(A)
##cacheSolve(speA)
##will return the cached inverse of a matrix

## First function create "special" matrix, it's the list of
## functions (set matrix, get, setsolve - make inverse of a matrix and 
## getsolve-get cache of inverse of a matrix).

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

## Second function get "special" matrix and test it - is there cache
## (is null or not). If not - function solve the matrix and write to cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
