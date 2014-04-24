## This function creates a special "matrix" object that can cache its inverse

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


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

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
