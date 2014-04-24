## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(z) {
                x <<- z
                m <<- NULL
        }
        get <- function() x
        setsolution <- function(solution) m <<- solution
        getsolution <- function() m
        list(
                set = set, 
                get = get,
                setsolution = setsolution,
                getsolution = getsolution)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolution()
        if (!is.null(m)) {
                message("retrieving cached data")
                return(m)
        }
        data <- x$get()
        m <- solution(data, ...)
        x$setsolution(m)
        m
}
