
## this function can catch the inverse

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


## this function will compute the inverse or retrieve it from thr cache.

cacheSolve <- function(x, ...) {
        In <- x$getin()
        if(!is.null(In)) {
                message("getting cached matrix")
                return(In)
        }
        data <- x$get()
        In <- solve(data, ...)
        x$setin(In)
        In
}
