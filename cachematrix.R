## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        In <- NULL
        set <- function(y) {
                x <<- y
                In <<- NULL
        }
        get <- function() x
        setin <- function(inverse) In <<- inverse
        getin <- function() In
        list(set = set, get = get,
             setin = setin,
             getin = getin)

}


## Write a short comnment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        In <- x$getin()
        if(!is.null(In)) {
                message("getting cached data")
                return(In)
        }
        data <- x$get()
        In <- solve(data, ...)
        x$setin(In)
        In
}
